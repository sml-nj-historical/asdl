(* sml-view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The "Cxx" view.
 *)

structure CxxView : sig

    val view : View.t

    structure File : VIEW_FILE_BASE
    structure Module : VIEW_MODULE_BASE

    structure Type : sig
	include VIEW_TYPE_BASE
	val getPublicCode : AST.TypeId.t -> string list
	val getProtectedCode : AST.TypeId.t -> string list
	val getPrivateCode : AST.TypeId.t -> string list
(* TODO: base_type *)
      end

    structure Constr : sig
	include VIEW_CONSTR_BASE
	val getPublicCode : AST.ConstrId.t -> string list
	val getProtectedCode : AST.ConstrId.t -> string list
	val getPrivateCode : AST.ConstrId.t -> string list
(* TODO: enum_value *)
      end

  end = struct

    structure CV = CommonView
    structure PN = PropNames
    structure PTy = PrimTypes

    structure ViewBase = ViewBaseFn (
      struct
	val viewName = "Cxx"
	val template =  {
		fileProps = #fileProps CV.template,
		moduleProps = #moduleProps CV.template,
		typeProps =
		  CV.prop(PN.base_type, false) ::
		  CV.prop(PN.public_code, true) ::
		  CV.prop(PN.protected_code, true) ::
		  CV.prop(PN.private_code, true) ::
		  #typeProps CV.template,
		consProps =
		  CV.prop(PN.public_code, true) ::
		  CV.prop(PN.protected_code, true) ::
		  CV.prop(PN.private_code, true) ::
		  CV.prop(PN.enum_value, false) ::
		  #consProps CV.template
	      }
	  fun mkFunName {operation, ty} = operation
      end)

    open ViewBase

    structure Type =
      struct
	open ViewBase.Type

	fun getCode prop id = View.getValues prop (view, View.Type id)

	val getPublicCode = getCode PN.public_code
	val getProtectedCode = getCode PN.protected_code
	val getPrivateCode = getCode PN.private_code
(* TODO: base_type *)
      end

    structure Constr =
      struct
	open ViewBase.Constr

	fun getCode prop id = View.getValues prop (view, View.Constr id)

	val getPublicCode = getCode PN.public_code
	val getProtectedCode = getCode PN.protected_code
	val getPrivateCode = getCode PN.private_code
(* TODO: enum_value *)
      end

  (* the default header template *)
    val header =
	  "// @FILENAME@\n\
	  \//\n\
	  \// Generated from @SRCFILE@ by asdl-gen.\n\
	  \//\n"

  (* set the default header property *)
    val () = let
	  val SOME prop = View.findProp(view, View.File, PN.header)
	  in
	    View.Prop.setValue(prop, header)
	  end

  (* set the default properties for the ASDL primitive types *)
    val () = let
	    fun set (id, propName, name) = let
		  val SOME prop = View.findProp(view, View.Type id, propName)
		  in
		    View.Prop.setValue(prop, name)
		  end
	    in
	      List.app set [
		  (PTy.boolTyId,	PN.name,	"bool"),
		  (PTy.intTyId,		PN.name,	"int"),
		  (PTy.uintTyId,	PN.name,	"unsigned int"),
		  (PTy.integerTyId,	PN.name,	"asdl::integer"),
		  (PTy.identifierTyId,	PN.name,	"asdl::identifier"),
		  (PTy.stringTyId,	PN.name,	"std::string"),
		  (PTy.tag8TyId,	PN.encoder,	"encode_tag8"),
		  (PTy.tag8TyId,	PN.decoder,	"decode_tag8"),
		  (PTy.tag16TyId,	PN.encoder,	"encode_tag16"),
		  (PTy.tag16TyId,	PN.decoder,	"decode_tag16")
		]
	    end

  (* set the default name for the ASDL primitive-types module *)
    val () = let
	    val primMod = View.Module PrimTypes.primTypesId
	    fun set (propName, name) = let
		  val SOME prop = View.findProp(view, primMod, propName)
		  in
		    View.Prop.setValue(prop, name)
		  end
	    in
	      List.app set [
		  (PN.name,		"asdl")
		]
	    end

  end
