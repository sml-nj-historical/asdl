(* view-base-fn.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature VIEW_BASE =
  sig

    val view : View.t

  end

functor ViewBaseFn (V : sig

    val viewName : string
    val template : View.template

  end) : VIEW_BASE = struct

    val view = View.new (V.viewName, V.template)

  end

