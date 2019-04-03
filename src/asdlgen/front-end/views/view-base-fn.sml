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

        local
          fun getCode prop modId = View.getOptValue prop (view, View.Module modId)
        in
	fun getInterfaceCode modId = {
		prologue = getCode PN.interface_prologue modId,
		epilogue = getCode PN.interface_epilogue modId
	      }
	fun getImplementationCode modId = {
		prologue = getCode PN.implementation_prologue modId,
		epilogue = getCode PN.implementation_epilogue modId
	      }
	end (* local *)
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
	val getReader = getFn (PN.reader, "read")
	val getWriter = getFn (PN.writer, "write")
	end (* local *)

	fun getNaturalType tyId = (
	      case View.getOptValue PN.natural_type (view, View.Type tyId)
	       of SOME[name] => name
		| _ => getName tyId
	      (* end case *))
        local
	  fun getFn prop = let
		val get = View.getOptValue prop
		in
		  fn id => (case get (view, View.Type id)
		       of SOME[name] => SOME name
			| _ => NONE
		      (* end case *))
		end
	in
	val getNaturalTypeCon = getFn PN.natural_type_con
	val getWrapper = getFn PN.wrapper
	val getUnwrapper = getFn PN.unwrapper
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

