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

  (* code inserts *)
    val getInterfaceCode : AST.ModuleId.t -> {
	    prologue : string list option,
	    epilogue : string list option
	  }
    val getImplementationCode : AST.ModuleId.t -> {
	    prologue : string list option,
	    epilogue : string list option
	  }

  end

signature VIEW_TYPE_BASE =
  sig

    val getName : AST.TypeId.t -> string

  (* writer/reader functions for memory/file pickling *)
    val getReader : AST.TypeId.t -> string
    val getWriter : AST.TypeId.t -> string

  (* returns the natural_type property if defined and otherwise behaves as getName *)
    val getNaturalType : AST.TypeId.t -> string
  (* type constructor that is applied to type *)
    val getNaturalTypeCon : AST.TypeId.t -> string option
  (* representation wrapper/unwrapper functions *)
    val getWrapper : AST.TypeId.t -> string option
    val getUnwrapper : AST.TypeId.t -> string option
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
