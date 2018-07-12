(* typecheck.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Typecheck : sig

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
    fun withMark' (cxt, {span, tree}) = (markCxt(cxt, span), tree)

    datatype token = S of string | A of Atom.atom | I of {tree : Atom.atom, span : Error.span}

    fun err ((errStrm, span), toks) = let
	  fun tok2str (S s) = s
	    | tok2str (A a) = Atom.toString a
	    | tok2str (I{tree, ...}) = Atom.toString tree
	  in
	    Error.errorAt(errStrm, span, List.map tok2str toks)
	  end

  (* a bogus type to return when there is an error *)
    val bogusTypeId = AST.TypeId.new (Atom.atom "**bogus**")
    val bogusType = AST.BaseTy bogusTypeId

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

  (* check a top-level definition (module, primitive module, or view) *)
    fun checkTop (cxt, env, PT.D_Mark m) =
	  checkTop (withMark (cxt, env, m))
      | checkTop (cxt, env, PT.D_Module{name, imports, decls}) = (
	  case Env.findModule(env, #tree name)
	   of SOME _ => (
		err (markCxt(cxt, #span name), [S "module '", I name, S "' is already defined"]);
		NONE)
	    | NONE => checkModule (cxt, env, #tree name, imports, decls)
	  (* end case *))
      | checkTop (cxt, env, PT.D_Primitive{name, exports}) = (
	  case Env.findModule(env, #tree name)
	   of SOME _ => (
		err (markCxt(cxt, #span name), [S "module '", I name, S "' is already defined"]);
		NONE)
	    | NONE => checkPrimModule (cxt, env, #tree name, exports)
	  (* end case *))
      | checkTop (cxt, env, PT.D_View{name, entries}) = raise Fail "FIXME: view"

    and checkModule (cxt, gEnv, name, imports, decls) = let
	  val id = AST.ModuleId.new name
	  in
	    Env.withModule (gEnv, id, fn env => let
	      val _ = List.app (fn im => checkImport(cxt, gEnv, env, im)) imports
	      val declsRef = ref[]
	      val module = AST.Module{
		      isPrim = false,
		      id = id,
		      decls = declsRef
		    }
	    (* first pass over decls defines mapping to type Ids *)
	      val tyIds = List.map (fn dcl => bindTyDecl (cxt, env, module, dcl)) decls
	      val decls = ListPair.mapPartial
		    (fn (tyId, dcl) => checkTyDecl (cxt, env, tyId, dcl))
		      (tyIds, decls)
	      in
		declsRef := decls;
		AST.ModuleId.bind(id, module);
		SOME module
	      end)
	  end

  (* typecheck a module's import declaration, where `gEnv` is the global environment
   * where we look up imports and `env` is the environment where we add them.
   *)
    and checkImport (cxt, gEnv, env, PT.Import_Mark{span, tree}) =
	  checkImport (markCxt(cxt, span), gEnv, env, tree)
      | checkImport (cxt, gEnv, env, PT.Import{module, alias}) = (
	  case Env.findModule (gEnv, #tree module)
	   of SOME id => (case alias
		 of NONE => Env.addImport(env, #tree module, id)
		  | SOME m => Env.addImport(env, #tree m, id)
		(* end case *))
	    | NONE => err (markCxt(cxt, #span module), [S "unknown module '", I module, S "'"])
	  (* end case *))

  (* first pass of checking a type declaration.  Here we create the unique type Id for the
   * defined type and add it to the environment with an empty rhs.  We return NONE if the
   * type is redefined or SOME id, where id is the type's unique Id.
   *)
    and bindTyDecl (cxt, env, module, tyDcl) = let
	(* check for possible redefination of the type *)
	  fun chkTyRedef (cxt, name) = (case Env.findType(env, NONE, name)
		 of SOME(AST.BaseTy _) => (
		      err (cxt, [S "redefinition of primitive type '", A name, S "'"]);
		      true)
		  | SOME _ =>  (
		      err (cxt, [S "redefinition of type '", A name, S "'"]);
		      true)
		  | NONE => false
		(* end case *))
	  fun bind ({span, tree=name}, mkDef) = if chkTyRedef (markCxt(cxt, span), name)
		then NONE
		else let
		  val id = AST.TypeId.new name
		  val dcl = AST.TyDcl{
			  id = id,
			  def = ref(mkDef()),
			  owner = module
			}
		  in
		  (* set the type Id's binding *)
		    AST.TypeId.bind (id, dcl);
		  (* add the type to the module's type environment *)
		    Env.insertType (env, name, dcl);
		    SOME id
		  end
	  fun isNullary (PT.Cons_Mark m) = isNullary(#tree m)
	    | isNullary (PT.Cons(_, [])) = true
	    | isNullary _ = false
	  in
	    case tyDcl
	     of PT.TD_Mark m => bindTyDecl (markCxt(cxt, #span m), env, module, #tree m)
	      | PT.TD_Sum{name, attribs, cons} =>
		  if List.null attribs andalso List.all isNullary cons
		    then bind (name, fn () => AST.EnumTy[])
		    else bind (name, fn () => AST.SumTy{attribs=[], cons=[]})
	      | PT.TD_Product{name, fields} => bind (name, fn () => AST.ProdTy{fields=[]})
	    (* end case *)
	  end

  (* typecheck a type declaration in a module.  Things to check for:
   *	- multiple definitions of constructor names
   *	- enumeration types
   *)
    and checkTyDecl (_, _, NONE, _) = NONE (* skip redefined types *)
      | checkTyDecl (cxt, env, optTyId, PT.TD_Mark{span, tree}) =
	  checkTyDecl (markCxt(cxt, span), env, optTyId, tree)
      | checkTyDecl (cxt, env, SOME tyId, tyDcl) = let
	  val SOME(dcl as AST.TyDcl{def, ...}) = AST.TypeId.bindingOf tyId
	  val owner = AST.LocalTy dcl
	(* check for recursive product types; reports an error and returns true
         * if there is a recursive chain of product types.
         *)
	  fun checkForRecTy tyId' = (case AST.TypeId.bindingOf tyId'
		 of SOME(AST.TyDcl{def=ref(AST.ProdTy{fields}), ...}) => let
		    (* check the type of a field *)
		      fun chk {label, ty} = let
			    fun chkId tyId'' = if AST.TypeId.same(tyId, tyId'')
				  then (
				    err (cxt, [
					S "recursive product type '", S(AST.TypeId.nameOf tyId),
					S "'"
				      ]);
				    true)
				  else checkForRecTy tyId''
			    in
			      case ty
			       of AST.Typ(AST.LocalTy(AST.TyDcl{id, ...})) => chkId id
				| AST.OptTy(AST.LocalTy(AST.TyDcl{id, ...})) => chkId id
				| AST.SeqTy(AST.LocalTy(AST.TyDcl{id, ...})) => chkId id
				| AST.SharedTy(AST.LocalTy(AST.TyDcl{id, ...})) => chkId id
				| _ => false
			      (* end case *)
			    end
		      in
			List.exists chk fields
		      end
		  | _ => false
		(* end case *))
	(* check if a constructor has already been defined in this scope *)
	  fun checkCons (cxt, cons) = (case Env.findCons (env, cons)
		 of SOME _ => (
		      err (cxt, [S "multiple definitions of constuctor '", A cons, S "'"]);
		      true)
		  | _ => false
		(* end case *))
	  in
	    case tyDcl
	     of PT.TD_Mark _ => raise Fail "impossible"
	      | PT.TD_Sum{name, attribs, cons} => (
		  case !def
		   of AST.EnumTy _ => let
		      (* check the constructors and add them to the environment *)
			fun chkCons (cxt, PT.Cons_Mark m) = chkCons(withMark'(cxt, m))
			  | chkCons (cxt, PT.Cons({span, tree}, _)) =
			      if checkCons (markCxt(cxt, span), tree)
				then NONE
				else let
				  val id = AST.ConsId.new tree
				  val constr = AST.Constr{id = id, owner = owner, fields = []}
				  in
				    Env.insertCons (env, id);
				    AST.ConsId.bind (id, constr);
				    SOME constr
				  end
			in
			  def := AST.EnumTy(List.mapPartial (fn cons => chkCons(cxt, cons)) cons)
			end
		    | AST.SumTy _ => let
(* TODO: need to check consistency of fields *)
		      (* check the common attributes *)
			val attribs' = checkFields (cxt, env, [], attribs)
		      (* check the constructors and add them to the environment *)
			fun chkCons (cxt, PT.Cons_Mark m) = chkCons(withMark'(cxt, m))
			  | chkCons (cxt, PT.Cons({span, tree}, fields)) =
			      if checkCons (markCxt(cxt, span), tree)
				then NONE
				else let
				  val id = AST.ConsId.new tree
				  val fields' = checkFields (cxt, env, attribs', fields)
				  val constr = AST.Constr{id = id, owner = owner, fields = fields'}
				  in
				    Env.insertCons (env, id);
				    AST.ConsId.bind (id, constr);
				    SOME constr
				  end
			in
			  def := AST.SumTy{
			      attribs = attribs',
			      cons = List.mapPartial (fn c => chkCons(cxt, c)) cons
			    }
			end
		    | _ => raise Fail "impossible"
		  (* end case *);
		  SOME dcl)
	      | PT.TD_Product{name, fields} => let
		  val fields' = checkFields (cxt, env, [], fields)
		  val rhs = AST.ProdTy{fields = fields'}
		  in
		    if checkForRecTy tyId
		      then NONE
		      else (
			def := rhs;
			SOME dcl)
		  end
	    (* end case *)
	  end

  (* check a list of fields (including checking for duplicate field names).  The
   * `attribs` list is a list of previously checked attributes (for sum types).
   *)
    and checkFields (cxt, env, attribs, fields) = let
	  fun chkField (cxt, PT.Field_Mark m) = chkField (withMark' (cxt, m))
	    | chkField (cxt, PT.Field{module, typ, tycon, label}) = let
		val ty = checkTy (cxt, env, module, typ, tycon)
		in
		  {label = Option.map (Atom.toString o #tree) label, ty = ty}
		end
	  in
(* FIXME: check for duplicate field names *)
	    attribs @ List.map (fn fld => chkField (cxt, fld)) fields
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
		  | NONE => (
		      err (cxt, [
			  S "unknown type '",
			  case modId
			   of SOME id => S(AST.ModuleId.nameOf id ^ ".")
			    | _ => S ""
			  (* end case *),
			  I typ, S "'"
			]);
		      AST.Typ bogusType)
		(* end case *))
	  in
	    case module
	     of SOME{span, tree} => (case Env.findModule(env, tree)
		   of NONE => (
			err (markCxt(cxt, span), [S "unknown module '", A tree, S "'"]);
			AST.Typ bogusType)
		    | someId => chkTy someId
		  (* end case *))
	      | NONE => chkTy NONE
	    (* end case *)
	  end

    and checkPrimModule (cxt, gEnv, name, exports) = let
	  val id = AST.ModuleId.new name
	  in
	    Env.withModule (gEnv, id, fn env => let
	      val declsRef = ref[]
	      val module = AST.Module{
		      isPrim = true,
		      id = id,
		      decls = declsRef
		    }
(* FIXME: check for duplicate exports *)
	      fun checkExport ({span, tree}, dcls) = let
		    val tyId = AST.TypeId.new tree
		    val dcl = AST.TyDcl{id = tyId, def = ref AST.PrimTy, owner = module}
		    in
		    (* set the type Id's binding *)
		      AST.TypeId.bind (tyId, dcl);
		    (* add the type to the module's type environment *)
		      Env.insertType (env, name, dcl);
		      dcl :: dcls
		    end
	      val decls = List.foldr checkExport [] exports
	      in
		declsRef := decls;
		AST.ModuleId.bind(id, module);
		SOME module
	      end)
	  end

    (* typechecker for an ASDL specification *)
    fun check { includes, file } = let
	  val env = Env.new()
	(* check an include file *)
	  fun checkInclude {name, errStrm, decls} =
		List.app (fn dcl => ignore (checkTop ((errStrm, (0, 0)), env, dcl))) decls
	(* check the ASDL specification file *)
	  val modules = let
		val {name, errStrm, decls} = file
		fun chk dcl = checkTop ((errStrm, (0, 0)), env, dcl)
		in
		  List.mapPartial chk decls
		end
	  in {
	    modules = modules
(* FIXME: what about the views? *)
	  } end

  end
