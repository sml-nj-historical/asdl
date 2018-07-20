(* view-base-fn.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor ViewBaseFn (V : sig

    val viewName : string
    val template : View.template

  end) : VIEW_BASE = struct

    val view = View.new (V.viewName, V.template)

    val getHeaderValue = View.getOptValue (Atom.atom "header")
    val getNameValue = View.getValue (Atom.atom "name")

    structure File =
      struct
	fun getHeader () = (
	      case getHeaderValue (view, View.File)
	       of NONE => []
		| SOME text => text
	      (* end case *))
      end

    structure Module =
      struct
	fun getName modId = (
	      case getNameValue (view, View.Module modId, AST.ModuleId.nameOf modId)
	       of [name] => name
		| _ => raise Fail "Module.getName"
	      (* end case *))
      end

    structure Type =
      struct
	fun getName id = (
	      case getNameValue (view, View.Type id, AST.TypeId.nameOf id)
	       of [name] => name
		| _ => raise Fail "Type.getName"
	      (* end case *))
      end

    structure Constr =
      struct
	fun getName id = (
	      case getNameValue (view, View.Constr id, AST.ConstrId.nameOf id)
	       of [name] => name
		| _ => raise Fail "Constr.getName"
	      (* end case *))
      end

  end

