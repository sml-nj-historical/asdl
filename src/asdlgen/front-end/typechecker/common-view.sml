(* common-view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * View properties that are common to all views
 *)

structure CommonView : sig

    val template : View.template

  (* `prop (name, accum)` constructs a property description *)
    val prop : string * bool -> View.Prop.desc

  end = struct

    fun prop (name, accum) = View.Prop.Desc{name = Atom.atom name, accumulator = accum}

    val template = {
	    moduleProps = List.map prop [
		("name", false)
	      ],
	    typeProps = List.map prop [
		("name", false),
		("natural_type", false),
		("encode", false),
		("decode", false),
		("writer", false),
		("reader", false)
	      ],
	    consProps = List.map prop [
		("name", false)
	      ]
	  }

  end


