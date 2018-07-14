(* sml-view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The "Sml" view.
 *)

structure SMLView : sig

    include VIEW_BASE

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

  end


