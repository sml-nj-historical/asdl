(* util.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Util : sig

  (* is a type an enumeration? *)
    val isEnum : AST.TypeId.t -> bool

  (* is a type represented by a pointer (i.e., boxed) or by an immediate value (unboxed)? *)
    val isBoxed : AST.TypeId.t -> bool

  (* names of components for tuples and records *)
    val fieldName : AST.label -> string
    val fieldGetName : AST.label -> string
    val fieldSetName : AST.label -> string

  (* return the representation type for a type expression *)
    val tyexpToCxx : AST.ty_exp -> Cxx.ty

  (* map a field to a C++ function parameter *)
    val fieldParam : AST.label -> Cxx.exp
    val fieldToParam : AST.field -> Cxx.param

  end = struct

    structure V = CxxView
    structure ModV = V.Module
    structure TyV = V.Type
    structure CL = Cxx

  (* get the type ID for a named_ty *)
    fun idOfNamedTy (AST.BaseTy id) = id
      | idOfNamedTy (AST.ImportTy(_, id)) = id
      | idOfNamedTy (AST.LocalTy(AST.TyDcl{id, ...})) = id

    fun isEnum tyId = (case AST.TypeId.bindingOf tyId
	   of SOME(AST.TyDcl{def = ref(AST.EnumTy _), ...}) => true
	    | SOME(AST.TyDcl{def = ref(AST.AliasTy(AST.Typ(nty, AST.NoTyc))), ...}) =>
		isEnum (idOfNamedTy nty)
	    | _ => false
	  (* end case *))

    fun isBoxed tyId = (case AST.TypeId.bindingOf tyId
	   of SOME(AST.TyDcl{def, ...}) => (case !def
		 of AST.EnumTy _ => false
		  | AST.AliasTy(AST.Typ(nty, AST.NoTyc)) => isBoxed (idOfNamedTy nty)
		  | AST.AliasTy _ => false (* options and sequences are not boxed *)
		  | AST.PrimTy => TyV.getBoxed tyId
		  | _ => true
		(* end case *))
	    | NONE => raise Fail(concat["Util.isBoxed(", AST.TypeId.nameOf tyId, ")"])
	  (* end case *))

    fun fieldName (AST.Pos i) = "_v" ^ Int.toString i
      | fieldName (AST.Lab lab) = "_" ^ lab

    fun fieldGetName (AST.Pos i) = "get_" ^ Int.toString i
      | fieldGetName (AST.Lab lab) = "get_" ^ lab

    fun fieldSetName (AST.Pos i) = "set_" ^ Int.toString i
      | fieldSetName (AST.Lab lab) = "set_" ^ lab

    fun tyexpToCxx (AST.Typ(ty, tyc)) = let
	  val (isBoxed, tyName) = (case ty
		 of AST.BaseTy tyId => (TyV.getBoxed tyId, TyV.getName tyId)
		  | AST.ImportTy(modId, tyId) =>
		      (isBoxed tyId, concat[ModV.getName modId, "::", TyV.getName tyId])
		  | AST.LocalTy(AST.TyDcl{id, def, ...}) => (case !def
		       of AST.EnumTy _ => (false, TyV.getName id)
			| AST.AliasTy(AST.Typ(nty, AST.NoTyc)) =>
			    (isBoxed (idOfNamedTy nty), TyV.getName id)
			| AST.AliasTy _ => (false, TyV.getName id)
			| AST.PrimTy => raise Fail "unexpected primitive type"
			| _ => (true, TyV.getName id)
		      (* end case *))
		(* end case *))
	  val cty = CL.T_Named tyName
	  in
	    case (isBoxed, tyc)
	     of (false, AST.NoTyc) => cty
	      | (true, AST.NoTyc) => CL.T_Ptr cty
	      | (false, AST.OptTyc) => CL.T_Template("asdl::option", [cty])
	      | (true, AST.OptTyc) => CL.T_Ptr cty
	      | (false, AST.SeqTyc) => CL.T_Template("std::vector", [cty])
	      | (true, AST.SeqTyc) => CL.T_Template("std::vector", [CL.T_Ptr cty])
	      | (_, AST.SharedTyc) => raise Fail "shared types are not yet supported"
	    (* end case *)
	  end

    fun fieldParamName (AST.Pos i) = "param" ^ Int.toString i
      | fieldParamName (AST.Lab lab) = lab

    fun fieldParam label = CL.mkVar(fieldParamName label)

    fun fieldToParam {label, ty} = CL.PARAM([], tyexpToCxx ty, fieldParamName label)

  end
