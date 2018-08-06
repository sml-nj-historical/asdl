(* ast.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

local

  structure ModuleId = IdentFn()
  structure TypeId = IdentFn()
  structure ConstrId = IdentFn()
  structure ViewId = IdentFn()

(* functor to add a definition property to a IDENTIFIER structure *)
  functor AddDefPropFn (
      type def
      structure Id : IDENTIFIER
    ) = struct

	type def = def

	open Id

	local
	  val {setFn, peekFn : t -> def option, ...} =
		newProp (fn id => raise Fail(concat[
		    "no definition for '", nameOf id, "'"
		  ]))
	in
	  val bind = setFn
	  val bindingOf = peekFn
	end (* local *)

    end

in

structure AST =
  struct

    datatype module = Module of {
	  isPrim : bool,			(* true for primitive modules *)
	  id : ModuleId.t,
	  decls : type_decl list ref
	}

    and type_decl = TyDcl of {
	  id : TypeId.t,
	  def : ty_def ref,
	  owner : module
	}

    and named_ty
      = BaseTy of TypeId.t
      | ImportTy of ModuleId.t * TypeId.t
      | LocalTy of type_decl

    and ty_def
      = EnumTy of constructor list
      | SumTy of {
	    attribs : field list,
	    cons : constructor list
	  }
      | ProdTy of {
	    fields : field list
	  }
      | AliasTy of ty_exp
      | PrimTy

    and constructor = Constr of {
	    id : ConstrId.t,
	    owner : named_ty,
	    fields : field list		(* fields of the constructor (includes attribs) *)
	  }

    and ty_exp
      = Typ of named_ty * tyc

    and tyc = NoTyc | OptTyc | SeqTyc | SharedTyc

  (* field labels are either positional or labelled *)
    and label = Pos of int | Lab of string

    withtype field = {
	  label : label,
	  ty : ty_exp
	}

    structure ModuleId = AddDefPropFn(
	type def = module
	structure Id = ModuleId)
    structure TypeId = AddDefPropFn(
	type def = type_decl
	structure Id = TypeId)
    structure ConstrId = AddDefPropFn(
	type def = constructor
	structure Id = ConstrId)

  (* debugging support *)
    fun tyToString (Typ(nty, tyc)) = let
	  val tyc = (case tyc
		 of NoTyc => ""
		  | OptTyc => "?"
		  | SeqTyc => "*"
		  | SharedTyc => "!"
		(* end case *))
	  in
	    case nty
	     of BaseTy id => TypeId.nameOf id ^ tyc
	      | ImportTy(modId, id) => concat[
		    ModuleId.nameOf modId, ".", TypeId.nameOf id, tyc
		  ]
	      | LocalTy(TyDcl{owner=Module{id=modId, ...}, id, ...}) => concat[
		    "(", ModuleId.nameOf modId, ")", TypeId.nameOf id, tyc
		  ]
	    (* end case *)
	  end

  end (* structure AST *)

end (* local *)
