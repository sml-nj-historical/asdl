(* sml-view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The "Cxx" view.
 *)

structure CxxView : sig

    val view : View.t

    structure Module : VIEW_MODULE_BASE
    structure Type : VIEW_TYPE_BASE
    structure Constr : VIEW_CONSTR_BASE

  end = struct

    structure CV = CommonView

    local
      structure ViewBase = ViewBaseFn (
	struct
	  val viewName = "Cxx"
	  val template =  {
		  moduleProps = #moduleProps CV.template,
		  typeProps =
		    CV.prop("base_type", false) ::
		    CV.prop("public_code", true) ::
		    CV.prop("protected_code", true) ::
		    CV.prop("private_code", true) ::
		    #typeProps CV.template,
		  consProps =
		    CV.prop("public_code", true) ::
		    CV.prop("protected_code", true) ::
		    CV.prop("private_code", true) ::
		    CV.prop("enum_value", false) ::
		    #consProps CV.template
		}
	end)
    in
    open ViewBase
    end

  end


