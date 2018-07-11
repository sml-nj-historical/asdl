(* prim-types.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for ASDL primitive types.
 *)

structure PrimTypes : sig

  (* primitive type IDs *)
    val boolTyId	: AST.TypeId.t
    val intTyId		: AST.TypeId.t
    val uintTyId	: AST.TypeId.t
    val integerTyId	: AST.TypeId.t
    val identifierTyId	: AST.TypeId.t
    val stringTyId	: AST.TypeId.t

  (* primitive types *)
    val boolTy		: AST.named_ty
    val intTy		: AST.named_ty
    val uintTy		: AST.named_ty
    val integerTy	: AST.named_ty
    val identifierTy	: AST.named_ty
    val stringTy	: AST.named_ty

  (* lookup a primitive type by name *)
    val find : Atom.atom -> AST.named_ty option

  end = struct

    structure TId = AST.TypeId

    val boolTyId	= TId.new (Atom.atom "bool")
    val intTyId		= TId.new (Atom.atom "int")
    val uintTyId	= TId.new (Atom.atom "uint")
    val integerTyId	= TId.new (Atom.atom "integer")
    val identifierTyId	= TId.new (Atom.atom "identifier")
    val stringTyId	= TId.new (Atom.atom "string")

    val boolTy		= AST.BaseTy boolTyId
    val intTy		= AST.BaseTy intTyId
    val uintTy		= AST.BaseTy uintTyId
    val integerTy	= AST.BaseTy integerTyId
    val identifierTy	= AST.BaseTy identifierTyId
    val stringTy	= AST.BaseTy stringTyId

  (* lookup a primitive type by name *)
    val find = let
	  val tbl = AtomTable.mkTable(8, Fail "prim-types")
	  fun ins (ty as AST.BaseTy id) = AtomTable.insert tbl (TId.atomOf id, ty)
	  in
	    List.app ins [
		boolTy,
		intTy,
		uintTy,
		integerTy,
		identifierTy,
		stringTy
	      ];
	    AtomTable.find tbl
	  end

  end
