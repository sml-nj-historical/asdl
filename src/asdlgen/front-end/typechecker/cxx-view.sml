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

    structure ViewBase = ViewBaseFn (
      struct
	val viewName = "Cxx"
	val template =  {
		fileProps = #fileProps CV.template,
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

    open ViewBase

    structure Type =
      struct
	open ViewBase.Type

	fun getCode prop id = View.getValues prop (view, View.Type id)

	val getPublicCode = getCode (Atom.atom "public_code")
	val getProtectedCode = getCode (Atom.atom "protected_code")
	val getPrivateCode = getCode (Atom.atom "private_code")
(* TODO: base_type *)
      end

    structure Constr =
      struct
	open ViewBase.Constr

	fun getCode prop id = View.getValues prop (view, View.Constr id)

	val getPublicCode = getCode (Atom.atom "public_code")
	val getProtectedCode = getCode (Atom.atom "protected_code")
	val getPrivateCode = getCode (Atom.atom "private_code")
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
	  val SOME prop = View.findProp(view, View.File, Atom.atom "header")
	  in
	    View.Prop.setValue(prop, header)
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
		  (PrimTypes.uintTyId,		"unsigned int"),
		  (PrimTypes.integerTyId,	"asdl::integer"),
		  (PrimTypes.identifierTyId,	"asdl::identifier"),
		  (PrimTypes.stringTyId,	"std::string")
		]
	    end

  end


