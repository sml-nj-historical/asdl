(* encoding.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A generic representation of the pickle encoding for an ASDL type declaration.
 *)

structure Encoding : sig

    datatype t
      = SWITCH of int * (int * AST.ConstrId.t * t option) list
      | TUPLE of t list
      | RECORD of (string * t) list
      | OPTION of base
      | SEQUENCE of base
      | SHARED of base
      | BASE of base

    withtype base = AST.ModuleId.t option * AST.TypeId.t	(* NONE for locally-defined types *)

    val encoding : AST.type_decl -> AST.TypeId.t * t

  end = struct

    datatype t
      = SWITCH of int * (int * AST.ConstrId.t * t option) list
      | TUPLE of t list
      | RECORD of (string * t) list
      | OPTION of base
      | SEQUENCE of base
      | SHARED of base
      | BASE of base

    withtype base = AST.ModuleId.t option * AST.TypeId.t	(* NONE for locally-defined types *)

    fun encoding (AST.TyDcl{id, def, ...}) = let
	  fun encTyExp (AST.Typ(ty, tyc)) = let
		val ty = (case ty
		       of AST.BaseTy tyId => (SOME PrimTypes.primTypesId, tyId)
			| AST.ImportTy(modId, tyId) => (SOME modId, tyId)
			| AST.LocalTy(AST.TyDcl{id, ...}) => (NONE, id)
		      (* end case *))
		in
		  case tyc
		   of AST.NoTyc => BASE ty
		    | AST.OptTyc => OPTION ty
		    | AST.SeqTyc => SEQUENCE ty
		    | AST.SharedTyc => SHARED ty
		  (* end case *)
		end
	  fun encFields (fields as {label=NONE, ty}::_) =
		TUPLE(List.map (fn {ty, ...} => encTyExp ty) fields)
	    | encFields fields =
		RECORD(List.map (fn {label=SOME lab, ty} => (lab, encTyExp ty)) fields)
	  fun encConstr (tag, AST.Constr{id, fields=[], ...}) = (tag, id, NONE)
	    | encConstr (tag, AST.Constr{id, fields, ...}) = (tag, id, SOME(encFields fields))
	  in
	    case !def
	     of AST.EnumTy cons => (id, SWITCH(length cons, List.mapi encConstr cons))
	      | AST.SumTy{cons, ...} => (id, SWITCH(length cons, List.mapi encConstr cons))
	      | AST.ProdTy{fields} => (id, encFields fields)
	      | AST.PrimTy => raise Fail "encoding: unexpected primitive type decl"
	    (* end case *)
	  end

  end
