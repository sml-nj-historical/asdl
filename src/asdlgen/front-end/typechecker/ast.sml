(* ast.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure AST =
  struct

    datatype module = Module of {
	  isPrim : bool,			(* true for primitive modules *)
	  name : string,
	  imports : module list,
	  decls : type_decl list ref
	}

    and type_decl = TyDcl of {
	  name : string,
	  def : ty_def ref,
	  owner : module
	}

    and named_ty
      = BaseTy of string
      | ImportTy of module * string
      | LocalTy of type_decl

    and ty_def
      = SumTy of {
	    attribs : field list,
	    cons : constructor list
	  }
      | ProdTy of {
	    fields : field list,
	  }
      | PrimTy

    and constructor = Constr of {
	    name : string,
	    owner : named_ty,
	    fields : field list
	  }

    and ty_exp
      = Typ of named_ty
      | OptTy of named_ty
      | SeqTy of named_ty
      | SharedTy of named_ty

    withtype field = {
	  label : string option,
	  ty : ty_exp
	}

    datatype view = View of {
	  name : string,
	  decls : view_decl list
	}

    and view_decl = VDcl of {
	  entity : view_entity,
	  prop : Atom.atom,
	  value : string
	}

    and view_entity
      = ModuleView of module
      | TypeView of module * named_ty
      | ConstrView of module * constructor
      | FieldView of module * named_ty * field

  end
