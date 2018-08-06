(* gen-types.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure GenTypes : sig

  (* generate the type definitions for an ASDL module.  The result will be
   * a namespace declaration enclosing the definitions.
   *)
    val gen : AST.module -> Cxx.decl

  end = struct

    structure V = CxxView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure CL = Cxx
    structure U = Util

  (* generate the inline access-methods for a field *)
    fun genAccessMethods {label, ty} = let
	  val fieldTy = U.tyexpToCxx ty
	  val field = CL.mkIndirect(CL.mkVar "this", U.fieldName label)
	  val get = CL.mkInlineMethDcl(
		fieldTy, U.fieldGetName label, [],
		CL.mkReturn(SOME field))
	  val set = CL.mkInlineMethDcl(
		CL.voidTy, U.fieldSetName label, [CL.param(fieldTy, "v")],
		CL.mkAssign(field, CL.mkVar "v"))
	  in
	    [get, set]
	  end

  (* generate a field declaration *)
    fun genField {label, ty} = CL.mkVarDcl(U.tyexpToCxx ty, U.fieldName label)

  (* a field initialization expression *)
    fun genFieldInit {label, ty} =
	  CL.mkApply(U.fieldName label, [U.fieldParam label])

  (* add view-property code to the list of class-body declarations *)
    fun addCode ([], dcls) = dcls
      | addCode (code, dcls) = dcls @ [CL.D_Verbatim code]

    fun gen (AST.Module{isPrim=false, id, decls}) = let
	  val namespace = ModV.getName id
	  val fwdDefs = List.map genForwardDcl (!decls)
	  val repDefs = List.foldr genType [] (!decls)
	  in
	    CL.D_Namespace(namespace, fwdDefs @ repDefs)
	  end
      | gen _ = raise Fail "GenTypes.gen: unexpected primitive module"

  (* generate a forward declaration for a type *)
    and genForwardDcl (AST.TyDcl{id, def, ...}) = let
	  val name = TyV.getName id
	  fun verb prefix = CL.D_Verbatim[concat[prefix, name, ";"]]
	  in
	    case !def
	     of AST.EnumTy _ => verb "enum class "
	      | AST.SumTy _ => verb "class "
	      | AST.ProdTy _ => verb "class "
	      | AST.AliasTy _ => CL.D_Verbatim[]
	      | AST.PrimTy => raise Fail "unexpected primitive type"
	    (* end case *)
	  end

    and genType (AST.TyDcl{id, def, ...}, dcls) = let
	  val name = TyV.getName id
	  in
	    case !def
	     of AST.EnumTy cons =>
		  genEnumClass (name, cons) @ dcls
	      | AST.SumTy{attribs, cons} =>
		  genBaseClass (id, name, attribs, cons) ::
		  List.foldr (genConsClass (name, attribs)) dcls cons
	      | AST.ProdTy{fields} =>
		  genProdClass (name, fields) :: dcls
	      | AST.AliasTy ty =>
		  CL.D_Typedef(name, U.tyexpToCxx ty) :: dcls
	      | AST.PrimTy => raise Fail "unexpected primitive type"
	    (* end case *)
	  end

  (* generate a enum-class definition and function declarations for an enumeration
   * type.
   *)
    and genEnumClass (name, cons) = let
	  val con::conr = List.map (fn (AST.Constr{id, ...}) => ConV.getName id) cons
	  val enumDcl = CL.D_EnumDef{
		  isClass = true,
		  name = name,
		  repTy = NONE,
		  cons = (con, SOME(CL.mkInt 1)) :: List.map (fn c => (c, NONE)) conr
		}
	(* prototypes for pickler functions *)
	  val protos = [] (* FIXME *)
	  in
	    enumDcl :: protos
	  end

  (* generate the base-class definition for a sum type *)
    and genBaseClass (tyId, name, attribs, cons) = let
	(* access methods for attribute fields *)
	  val accessMeths = List.foldr
		(fn (fld, meths) => genAccessMethods fld @ meths)
		  [] attribs
	(* pickling/unpickling methods *)
	  val pickleMeth = CL.mkVirtualProto(
		CL.voidTy, "encode",
		[CL.param(CL.T_Ref(CL.T_Named "asdl::outstream"), "os")],
		true (* abstract method *))
	  val unpickleMeth = CL.mkStaticMethProto(
		CL.T_Ptr(CL.T_Named name), "decode",
		[CL.param(CL.T_Ref(CL.T_Named "asdl::instream"), "is")])
	(* type definition for tag values *)
	  val tagTypeDcl = CL.D_EnumDef{
		  isClass = false,
		  name = "_tag_t",
		  repTy = NONE,
		  cons = List.map
		    (fn (AST.Constr{id, ...}) => (U.constrTagName id, NONE))
		      cons
		}
	  val tagTy = CL.T_Named "_tag_t"
	(* tag field *)
	  val tagDcl = CL.mkVarDcl(tagTy, U.tagFieldName)
	(* create the constructor function *)
	  val initTag = CL.mkApply(U.tagFieldName, [CL.mkVar "tag"])
	  val initAttribs = List.map genFieldInit attribs
	  val constr = CL.mkConstrDcl(
		name,
		CL.param(tagTy, "tag") :: List.map U.fieldToParam attribs,
		initTag :: initAttribs, CL.mkBlock[])
	(* destructor *)
	  val destr = CL.D_Destr(["virtual"], [], name, NONE)
	  in
	    CL.D_ClassDef{
		name = name, args = NONE, from = NONE,
		public = destr ::
		  pickleMeth ::
		  unpickleMeth ::
		  addCode(TyV.getPublicCode tyId, accessMeths),
		protected = tagTypeDcl ::
		  constr ::
		  tagDcl ::
		  addCode(TyV.getProtectedCode tyId, List.map genField attribs),
		private = addCode(TyV.getPrivateCode tyId, [])
	      }
	  end

  (* generate a derived-class definition for a constructor in a sum type *)
    and genConsClass (baseName, attribs) = let
	  val nAttribs = List.length attribs
	  val attribParams = List.map U.fieldToParam attribs
	  val attribInit = List.map (U.fieldParam o #label) attribs
	  fun baseInit con = CL.mkApply(
		baseName,
		CL.mkVar(concat[baseName, "::", U.constrTagName con]) :: attribInit)
	  fun gen (AST.Constr{id, fields, ...}, dcls) = let
		val name = ConV.getName id
		val extra = List.drop(fields, nAttribs)
		val accessMeths = List.foldr
		      (fn (fld, meths) => genAccessMethods fld @ meths)
			[] extra
	      (* pickling method *)
		val pickleMeth = CL.mkMethProto(
		      CL.voidTy, "encode",
		      [CL.param(CL.T_Ref(CL.T_Named "asdl::outstream"), "os")])
		val constr = CL.mkConstrDcl(
		      name, List.map U.fieldToParam fields,
		      baseInit id :: List.map genFieldInit extra, CL.mkBlock[])
(* FIXME: eventually we need something better for the destructor *)
		val destr = CL.D_Destr([], [], name, SOME(CL.mkBlock[]))
		in
		  CL.D_ClassDef{
		      name = name, args = NONE, from = SOME baseName,
		      public = constr ::
			destr ::
			pickleMeth ::
			addCode (ConV.getPublicCode id, accessMeths),
		      protected = addCode (ConV.getProtectedCode id, List.map genField attribs),
		      private = addCode (ConV.getPrivateCode id, [])
		    } :: dcls
		end
	  in
	    gen
	  end

  (* generate the class definition for a product type *)
    and genProdClass (name, fields) = let
	  val accessMeths = List.foldr
		(fn (fld, meths) => genAccessMethods fld @ meths)
		  [] fields
	(* pickling/unpickling methods *)
	  val pickleMeth = CL.mkMethProto(
		CL.voidTy, "encode",
		[CL.param(CL.T_Ref(CL.T_Named "asdl::outstream"), "os")])
	  val unpickleMeth = CL.mkStaticMethProto(
		CL.T_Ptr(CL.T_Named name), "decode",
		[CL.param(CL.T_Ref(CL.T_Named "asdl::instream"), "is")])
	(* create the constructor function *)
	  val constr = CL.mkConstrDcl(
		name, List.map U.fieldToParam fields,
		List.map genFieldInit fields, CL.mkBlock[])
(* FIXME: eventually we need something better for the destructor *)
	  val destr = CL.D_Destr([], [], name, SOME(CL.mkBlock[]))
	  in
	    CL.D_ClassDef{
		name = name, args = NONE, from = NONE,
		public = constr ::
		  destr ::
		  pickleMeth ::
		  unpickleMeth ::
		  accessMeths,
		protected = [],
		private = List.map genField fields
	      }
	  end

  end


