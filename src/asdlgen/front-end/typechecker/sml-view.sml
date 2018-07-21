(* sml-view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The "Sml" view.
 *)

structure SMLView : sig

    val view : View.t

    structure File : VIEW_FILE_BASE
    structure Module : VIEW_MODULE_BASE
    structure Type : VIEW_TYPE_BASE
    structure Constr : VIEW_CONSTR_BASE

  end = struct

    local
      structure ViewBase = ViewBaseFn (
	struct
	  val viewName = "Sml"
	  val template = CommonView.template
	  fun mkFunName {operation, ty} = String.concat[operation, ty]
	end)
    in
    open ViewBase
    end

  (* the default header template *)
    val header =
	  "(* @FILENAME@\n\
	  \ *\n\
	  \ * Generated from @SRCFILE@ by asdl-gen.\n\
	  \ *)\n"

  (* set the default header property *)
    val () = let
	  val SOME prop = View.findProp(view, View.File, Atom.atom "header")
	  in
	    View.Prop.setValue(prop, header)
	  end

  (* set the default names for the ASDL primitive types *)
    val () = let
	    fun set (id, name) = let
		  val SOME prop = View.findProp(view, View.Type id, PropNames.name)
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
