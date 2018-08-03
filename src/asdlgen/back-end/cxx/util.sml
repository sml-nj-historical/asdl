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
    val tyexpToCxx : AST.ty_exp -> CL.ty

  end = struct

    structure V = CxxView
    structure ModV = V.Module
    structure TyV = V.Type
    structure CL = CLang

    fun isEnum tyId = (case AST.TypeId.bindingOf tyId
	   of (AST.TyDcl{def = ref(AST.EnumTy _), ...}) => true
	    | _ => false
	  (* end case *))

    fun isBoxed tyId = not(isEnum tyId)
	  andalso ??

    fun fieldName (AST.Pos i) = "_v" ^ Int.toString i
      | fieldName (AST.Lab lab) = "_" ^ lab

    fun fieldGetName (AST.Pos i) = "get_" ^ Int.toString i
      | fieldGetName (AST.Lab lab) = "get_" ^ lab

    fun fieldSetName (AST.Pos i) = "set_" ^ Int.toString i
      | fieldSetName (AST.Lab lab) = "set_" ^ lab

    fun tyexpToCxx (AST.Typ(ty, tyc)) = let
	  val (isBoxed, tyName) = (case ty
		 of AST.BaseTy tyId => (??, TyV.getName tyId)
		  | AST.ImportTy(modId, tyId) =>
		      (isBoxed tyId, concat[ModV.getName modId, "::", TyV.getName tyId])
		  | AST.LocalTy(AST.TyDcl{id, def, ...}) => (case !def
		       of AST.EnumTy => (false, TyV.getName tyId)
			| AST.PrimTy _ => raise Fail "unexpected primitive type"
			| _ => (true, TyV.getName tyId)
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

  end
