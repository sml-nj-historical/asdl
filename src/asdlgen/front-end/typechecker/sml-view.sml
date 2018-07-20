(* sml-view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The "Sml" view.
 *)

structure SMLView : sig

    val view : View.t

    structure Module : VIEW_MODULE_BASE
    structure Type : VIEW_TYPE_BASE
    structure Constr : VIEW_CONSTR_BASE

  end = struct

    local
      structure ViewBase = ViewBaseFn (
	struct
	  val viewName = "Sml"
	  val template = CommonView.template
	end)
    in
    open ViewBase
    end

  (* set the default names for the ASDL primitive types *)
    val () = let
	    fun set (id, name) = let
		  val SOME prop = View.findProp(view, View.Type id, Atom.atom "name")
		  in
		    View.Prop.setValue(prop, name)
		  end
	    in
	      List.app set [
		  (PrimTypes.boolTyId,		"bool"),
		  (PrimTypes.intTyId,		"int"),
		  (PrimTypes.uintTyId,		"word"),
		  (PrimTypes.integerTyId,	"IntInf.int"),
		  (PrimTypes.identifierTyId,	"Atom.atom"),
		  (PrimTypes.stringTyId,	"string")
		]
	    end

  end
