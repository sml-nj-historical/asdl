(* view-base-sig.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature VIEW_MODULE_BASE =
  sig

    val getName : AST.ModuleId.t -> string

  end

signature VIEW_TYPE_BASE =
  sig

    val getName : AST.TypeId.t -> string

  end

signature VIEW_CONSTR_BASE =
  sig

    val getName : AST.ConstrId.t -> string

  end

signature VIEW_BASE =
  sig

    val view : View.t

    structure Module : VIEW_MODULE_BASE
    structure Type : VIEW_TYPE_BASE
    structure Constr : VIEW_CONSTR_BASE

  end
