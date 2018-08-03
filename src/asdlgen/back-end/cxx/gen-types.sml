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

  (* generate code for a constructor *)
    fun defConstr tyName (cons, cd : code) = let

	(* the class definition for the constructor *)
	  val cls = CL.D_ClassDef{
		  name = Cons.nameOf(view, cons),
		  args = NONE,
		  from = SOME tyName,
		  public = ??,
		  protected = [],
		  private = fields
		}
	  in
	    {cls :: #hxx cd, fns @ #cxx cd}
	  end

    fun gen (AST.Module{isPrim=false, id, decls}) = let
	  val namespace = ModV.getName id
	  val fwdDefs = List.map genForwardDcl decls
	  val repDefs = List.map genType decls
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

    and genType (AST.TyDcl{id, def, ...}) = let
	  val name = Util.repName(TyV.getName id)
	  fun db cons = let
		fun con (AST.Constr{id, fields=[], ...}) = (ConV.getName id, NONE)
		  | con (AST.Constr{id, fields, ...}) = (ConV.getName id, SOME(genProdTy fields))
		in
		  (S.DB([], name, List.map con cons)::dbs, tbs)
		end
	  in
	    case !def
	     of AST.EnumTy cons => db cons
	      | AST.SumTy{attribs, cons} => db cons
	      | AST.ProdTy{fields} => (dbs, ([], name, genProdTy fields)::tbs)
	      | AST.PrimTy => raise Fail "unexpected primitive type"
	    (* end case *)
	  end

  (* generate a type expression for a list of fields *)
    and genProdTy [] = S.CONty([], "unit")
      | genProdTy [{label=NONE, ty}] = genTyExp ty
      | genProdTy (fields as {label=NONE, ...}::_) =
	  S.TUPLEty(List.map (genTyExp o #ty) fields)
      | genProdTy fields = let
	  fun field {label=SOME lab, ty} = (lab, genTyExp ty)
	    | field _ = raise Fail "missing label in record type"
	  in
	    S.RECORDty(List.map field fields)
	  end

    and genTyExp (AST.Typ(ty, tyc)) = let
	  val tyName = (case ty
		 of AST.BaseTy tyId => TyV.getName tyId
		  | AST.ImportTy(modId, tyId) => String.concat[
			ModV.getName modId, ".", TyV.getName tyId
		      ]
		  | AST.LocalTy(AST.TyDcl{id, ...}) => TyV.getName id
		(* end case *))
	  val ty' = S.CONty([], tyName)
	  in
	    case tyc
	     of AST.NoTyc => ty'
	      | AST.OptTyc => S.CONty([ty'], "option")
	      | AST.SeqTyc => S.CONty([ty'], "list")
	      | AST.SharedTyc => raise Fail "FIXME: shared types not implemented"
	    (* end case *)
	  end

  end


