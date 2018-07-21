(* view-base-fn.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor ViewBaseFn (V : sig

    val viewName : string
    val template : View.template

  (* target-specific name mangling for the operation (read/write/encode/decode)
   * and type name.
   *)
    val mkFunName : {operation : string, ty : string} -> string

  end) : VIEW_BASE = struct

    structure PN = PropNames

    val view = View.new (V.viewName, V.template)

    val getHeaderValue = View.getOptValue PropNames.header
    val getNameValue = View.getValue PropNames.name

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
	local
	  fun getFn (prop, operation) = let
		val get = View.getOptValue prop
		in
		  fn id => (case get (view, View.Type id)
		       of SOME[name] => name
			| _ => V.mkFunName{operation=operation, ty=AST.TypeId.nameOf id}
		      (* end case *))
		end
	in
	val getEncoder = getFn (PN.encoder, "encode")
	val getDecoder = getFn (PN.decoder, "decode")
	val getReader = getFn (PN.reader, "read")
	val getWriter = getFn (PN.writer, "write")
	end (* local *)
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

