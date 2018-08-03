(* gen-types.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure GenTypes : sig

  (* generate the type definitions for an ASDL module.  The result will be
   * a namespace declaration enclosing the definitions.
   *)
    val genDcls : AST.module -> CL.decl

  end = struct

    structure V = CxxView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure CL = CLang
    structure U = Util

  (* generate the inlinw access-methods for a field *)
    fun genAccessMethods {label, ty} = let
	  val fieldTy = U.tyexpToCxx ty
	  val field = CL.mkIndirect(CL.mkVar "this", U.fieldName label)
	  val get = CL.D_Func(
		[], fieldTy, [], U.fieldGetName label, [],
		CL.mkReturn(SOME field))
	  val set = CL.D_Func(
		[], CL.voidTy, [], U.fieldSetName label, [CL.PARAM([], fieldTy, "v")],
		CL.mkAssign(field, CL.mkVar "v"))
	  in
	    [get, set]
	  end

  (* generate a field declaration *)
    fun genField {label, ty} = CL.mkVarDcl(U.tyexpToCxx ty, U.fieldName label)

    fun gen (AST.Module{isPrim=false, id, decls}) = let
	  val namespace = ModV.getName id
	  val fwdDefs = List.map genForwardDcl decls
	  val repDefs = List.foldr genType [] decls
	  in
	    CL.D_Namespace(namespace, fwdDefs @ repDefs)
	  end
      | gen _ = raise Fail "genDcls: unexpected primitive module"

  (* generate a forward declaration for a type *)
    and genForwardDcl (AST.TyDcl{id, def, ...}) = let
	  val name = TyV.getName id
	  val prefix = (case !def
		 of AST.EnumTy _ => "enum class "
		  | AST.SumTy _ => "class "
		  | AST.ProdTy _ => "class "
		  | AST.PrimTy => raise Fail "unexpected primitive type"
		(* end case *))
	  in
	    CL.D_Verbatim[concat[prefix, name, ";\n"]]
	  end

    and genType (AST.TyDcl{id, def, ...}, dcls) = let
	  val name = U.repName(TyV.getName id)
	  fun db cons = let
		fun con (AST.Constr{id, fields=[], ...}) = (ConV.getName id, NONE)
		  | con (AST.Constr{id, fields, ...}) = (ConV.getName id, SOME(genProdTy fields))
		in
		  (S.DB([], name, List.map con cons)::dbs, tbs)
		end
	  in
	    case !def
	     of AST.EnumTy cons =>
		  genEnumClass (name, cons) :: dcls
	      | AST.SumTy{attribs, cons} =>
		  genBaseClass (name, attribs) ::
		  List.foldr (genConsClass (name, attribs)) dcls cons
	      | AST.ProdTy{fields} =>
		  genProdClass (name, fields) :: dcls
	      | AST.PrimTy => raise Fail "unexpected primitive type"
	    (* end case *)
	  end

  (* generate a enum-class definition and function declarations for an enumeration
   * type.
   *)
    and genEnumClass (name, cons) =

  (* generate the base-class definition for a sum type *)
    and genBaseClass (name, attribs) = let
	  val accessMeths = List.foldr
		(fn (fld, meths) => genAccessMethods fld @ meths)
		  [] attribs
	  in
	    CL.D_ClassDef{
		name = name, args = NONE, from = NONE,
		public = accessMeths,
		protected = constr :: destr :: List.map genField attribs,
		private = []
	      }
	  end

  (* generate a derived-class definition for a constructor in a sum type *)
    and genConsClass (name, attribs) (AST.Constr{id, fields, ...}) =

  (* generate the class definition for a product type *)
    and genProdClass (name, fields) = let
	  val accessMeths = List.foldr
		(fn (fld, meths) => genAccessMethods fld @ meths)
		  [] attribs
	  in
	    CL.D_ClassDef{
		name = name, args = NONE, from = NONE,
		public = constr :: destr :: accessMeths,
		protected = [],
		private = List.map genField attribs
	      }
	  end

  end


