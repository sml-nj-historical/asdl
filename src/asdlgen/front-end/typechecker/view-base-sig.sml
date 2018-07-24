(* view-base-sig.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature VIEW_FILE_BASE =
  sig

    val getHeader : unit -> string list

  end

signature VIEW_MODULE_BASE =
  sig

  (* get the name of the module *)
    val getName : AST.ModuleId.t -> string

  end

signature VIEW_TYPE_BASE =
  sig

    val getName : AST.TypeId.t -> string
    val getEncoder : AST.TypeId.t -> string
    val getDecoder : AST.TypeId.t -> string
    val getReader : AST.TypeId.t -> string
    val getWriter : AST.TypeId.t -> string

  end

signature VIEW_CONSTR_BASE =
  sig

    val getName : AST.ConstrId.t -> string

  end

signature VIEW_BASE =
  sig

    val view : View.t

    structure File : VIEW_FILE_BASE
    structure Module : VIEW_MODULE_BASE
    structure Type : VIEW_TYPE_BASE
    structure Constr : VIEW_CONSTR_BASE

  end
