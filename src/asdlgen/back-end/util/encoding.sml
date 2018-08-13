(* encoding.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A generic representation of the pickle encoding for an ASDL type declaration.
 *)

structure Encoding : sig

  (* the pickle encoding of an ASDL type *)
    datatype t
    (* single-constructor enumeration *)
      = UNIT of AST.ConstrId.t
    (* enumeration: constructors are indexed from 0 *)
      | ENUM of int * (int * AST.ConstrId.t) list
    (* single-constructor non-enumeration sum type *)
      | WRAP of AST.ConstrId.t * obj
    (* non-enumeration sum type with optional attributes; the constructor encodings
     * do not include the attributes.
     *)
      | SWITCH of obj option * int * (int * AST.ConstrId.t * obj option) list
    (* product type *)
      | OBJ of obj
    (* alias type *)
      | ALIAS of tyexp

  (* encoding of product types *)
    and obj
      = TUPLE of (int * tyexp) list		(* unlabeled fields; indexed from 0 *)
      | RECORD of (string * tyexp) list		(* labeled fields *)

  (* type expressions *)
    and tyexp
      = OPTION of base
      | SEQUENCE of base
      | SHARED of base
      | BASE of base

    withtype base = AST.ModuleId.t option * AST.TypeId.t	(* NONE for locally-defined types *)

  (* determine the encoding of an ASDL type *)
    val encoding : AST.type_decl -> AST.TypeId.t * t

  (* determine the pickle representation "type" of a tag for the given number of constructors  *)
    val tagTyId : int -> AST.TypeId.t

  (* prefix a constructor argument with optional attributes *)
    and prefixWithAttribs : obj option * obj option -> obj option

  end = struct

    datatype t
      = UNIT of AST.ConstrId.t
      | ENUM of int * (int * AST.ConstrId.t) list
      | WRAP of AST.ConstrId.t * obj
      | SWITCH of obj option * int * (int * AST.ConstrId.t * obj option) list
      | OBJ of obj
      | ALIAS of tyexp

    and obj
      = TUPLE of (int * tyexp) list
      | RECORD of (string * tyexp) list

    and tyexp
      = OPTION of base
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
	  fun encFields (base, fields as {label=AST.Pos _, ty}::_) =
		TUPLE(List.mapi (fn (i, {ty, ...}) => (base+i, encTyExp ty)) fields)
	    | encFields (_, fields) =
		RECORD(List.map (fn {label=AST.Lab lab, ty} => (lab, encTyExp ty)) fields)
	  fun encEnumConstr (tag, AST.Constr{id, ...}) = (tag, id)
	  fun encConstr nAttribs (tag, AST.Constr{id, fields, ...}) = (
		case List.drop(fields, nAttribs)
		 of [] => (tag, id, NONE)
		  | fields => (tag, id, SOME(encFields (nAttribs, fields)))
		(* end case *))
	  in
	    case !def
	     of AST.EnumTy[AST.Constr{id=conId, ...}] => (id, UNIT conId)
	      | AST.EnumTy cons => (id, ENUM(length cons, List.mapi encEnumConstr cons))
	      | AST.SumTy{cons=[AST.Constr{id=conId, fields, ...}], ...} =>
		  (id, WRAP(conId, encFields (0, fields)))
	      | AST.SumTy{attribs=[], cons} =>
		  (id, SWITCH(NONE, length cons, List.mapi (encConstr 0) cons))
	      | AST.SumTy{attribs, cons} => let
		  val enc = SWITCH(
			SOME(encFields (0, attribs)),
			length cons,
			List.mapi (encConstr (length attribs)) cons)
		  in
		    (id, enc)
		  end
	      | AST.ProdTy{fields} => (id, OBJ(encFields (0, fields)))
	      | AST.AliasTy ty => (id, ALIAS(encTyExp ty))
	      | AST.PrimTy => raise Fail "encoding: unexpected primitive type decl"
	    (* end case *)
	  end

    fun tagTyId ncons = if (ncons <= 256) then PrimTypes.tag8TyId
	  else if (ncons <= 65536) then PrimTypes.tag16TyId
	  else raise Fail "too many constructors"

    fun prefixWithAttribs (NONE, arg) = arg
      | prefixWithAttribs (attribs, NONE) = attribs
      | prefixWithAttribs (SOME(TUPLE flds1), SOME(TUPLE flds2)) = SOME(TUPLE(flds1 @ flds2))
      | prefixWithAttribs (SOME(RECORD flds1), SOME(RECORD flds2)) = SOME(RECORD(flds1 @ flds2))
      | prefixWithAttribs _ = raise Fail "Encoding.prefixWithAttribs: inconsistent product types"

  end
