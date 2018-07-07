(* typecheck.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Typecheck : sig

  (* the contents of an ASDL source file *)
    type file_content = { file : string, content : ParseTree.decl list }

    val check : {
	    includes : Parser.file list,
	    file : Parser.file
	  } -> {
	    modules : AST.module list
(* some other stuff too *)
	  }

  end = struct

    structure PT = ParseTree

    fun markCxt ((errStrm, _), span) = (errStrm, span)

    fun withMark (cxt, env, {span, tree}) = (markCxt(cxt, span), env, tree)

    datatype token = S of string | A of Atom.atom

    fun err ((errStrm, span), toks) = let
	  fun tok2str (S s) = s
	    | tok2str (A a) = Atom.toString a
	  in
	    Error.errorAt(errStrm, span, List.map tok2str toks)
	  end

  (* check for duplicate names in a list of named values *)
    fun anyDups getName = let
	  fun test a = let val a' = getName a in fn b => Atom.same(a', getName b) end
	  fun chk [] = NONE
	    | chk (x::xs) = if List.exists (test x) xs
		then SOME x
		else chk xs
	  in
	    chk
	  end

    fun checkTopDecl (cxt, env, PT.D_Mark m) =
	  checkTopDecl (withMark (cxt, env, m))
      | checkTopDecl (cxt, env, PT.D_Module{name, imports, decls}) = (
	  case Env.findModule(env, name)
	   of SOME _ => (* error: duplicate module definition *)
	    | NONE => ??
	  (* end case *))
      | checkTopDecl (cxt, env, PT.D_Primitive{name, exports}) = (
	  case Env.findModule(env, name)
	   of SOME _ => (* error: duplicate module definition *)
	    | NONE => ??
	  (* end case *))
      | checkTopDecl (cxt, env, PT.D_View{name, entries}) =

    and checkModule (cxt, gEnv, name, imports, decls) = let
	  val id = AST.ModuleId.new (Atom.toString name)
	  in
	    Env.withModule (gEnv, id, fn env => let
	      val _ = List.app (fn im => checkImport(cxt, gEnv, env, im)) imports
	      val declsRef = ref[]
	      val module = AST.Module{
		      isPrim = false,
		      id = id,
		      decls = declsRef
		    }
	      val decls = List.mapPartial checkTypeDecl decls
	      in
		declsRef := decls;
		AST.ModuleId.bind(id, module);
		module
	      end)
	  end

  (* typecheck a module's import declaration, where `gEnv` is the global environment
   * where we look up imports and `env` is the environment where we add them.
   *)
    and checkImport (cxt, gEnv, env, PT.Import_Mark{span, tree}) =
	  checkImport (markCxt(cxt, span), gEnv, env, tree)
      | checkImport (cxt, gEnv, env, PT.Import{module, alias}) = (
	  case Env.findModule (gEnv, module)
	   of SOME id => (case alias
		 of NONE => Env.addImport(env, module, id)
		  | SOME m => Env.addImport(env, m, id)
		(* end case *))
	    | NONE => err (cxt, [S "unknown module '", A module, S "'"])
	  (* end case *))

  (* typecheck a type declaration in a module.  Things to check for:
   *	- multiple definitions of same type name
   *	- multiple definitions of constructor names
   *	- enumeration types
   *)
    and checkTyDecl (cxt, env, tyDcl) = let
	  in
	    case tyDcl
	     of PT.TD_Mark m => checkTyDecl (withMark (cxt, env, m))
	      | PT.TD_Sum{name, attribs, cons} =>
	      | PT.TD_Product{name, fields} =>
	    (* end case *)
	  end

  (* check a list of fields (including checking for duplicate field names).  The
   * `attribs` list is a list of previously checked attributes (for sum types).
   *)
    and checkFields (cxt, env, attribs, fields) = let
	  fun chkField (cxt, env, PT.Field_Mark m) = chkField (withMark (cxt, env, m))
	    | chkField (cxt, env, PT.Field{module, typ, tycon, label}) = ??
	  in
	  end

  (* check a type expression, where the module name and tycon are optional *)
    and checkTy (cxt, env, module, typ : PT.id, tycon) = let
	  fun chkTy modId = (case Env.findType (env, modId, #tree typ)
		 of SOME ty => (case tycon
		       of NONE => AST.Typ ty
			| SOME PT.Optional => AST.OptTy ty
			| SOME PT.Sequence => AST.SeqTy ty
			| SOME PT.Shared => AST.SharedTy ty
		      (* end case *))
		  | NONE => ??
		(* end case *))
	  in
	    case module
	     of SOME{span, tree} => (case Env.findModule(env, tree)
		   of NONE => (
			err (markCxt(cxt, span), [S "unknown module '", A tree, "'"]);
			bogusType)
		    | someId => chkTy someId
		  (* end case *))
	      | NONE => chkTy NONE
	    (* end case *)
	  end

  end
