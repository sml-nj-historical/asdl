(* gen-pickle.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the implementation for an ASDL module.  The implementation includes
 * the destructor functions for boxed types and the encoder/decoder functions.
 *
 * For a  boxed ASDL type "TYPE", the encoder is a method with the following
 * signature:
 *
 *	void write (asdl::outstream &os);
 *
 * and the decoder is a static class method with the following signature:
 *
 *	TYPE *read (asdl::instream &is);
 *
 * For enumeration and alias types, we generate the functions:
 *
 *	void write_TYPE (asdl::outstream &os, TYPE v);
 *	TYPE read_TYPE (asdl::instream &is);
 *
 * TODO:
 * 	write/decoder names should be taken from view
 *	proper implementation of destructor functions
 *)

structure GenPickle : sig

  (* generate the implementation of the ASDL module.  The result will be
   * a namespace declaration enclosing the function/method definitions.
   *)
    val gen : AST.module -> Cxx.decl

  end = struct

    structure PT = PrimTypes
    structure V = CxxView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure E = Encoding
    structure CL = Cxx
    structure U = Util

    val osParam = CL.param(CL.T_Ref(CL.T_Named "asdl::outstream"), "os")
    val osArg = CL.mkVar "os"
    val isParam = CL.param(CL.T_Ref(CL.T_Named "asdl::instream"), "is")
    val isArg = CL.mkVar "is"

  (* pickler function name for primitive types *)
    fun baseWriter (NONE, tyId) = TyV.getWriter tyId
      | baseWriter (SOME modId, tyId) = concat[
	    ModV.getName modId, "::", TyV.getWriter tyId
	  ]

  (* unpickler function name for primitive types *)
    fun baseReader (NONE, tyId) = TyV.getReader tyId
      | baseReader (SOME modId, tyId) = concat[
	    ModV.getName modId, "::", TyV.getReader tyId
	  ]

  (* invoke the pickler operation on the argument *)
    fun write (optModId, tyId, arg) = if U.isBoxed tyId
	  then CL.mkExpStm(CL.mkIndirectDispatch (arg, "write", [osArg]))
	  else CL.mkCall (baseWriter(optModId, tyId), [osArg, arg])

  (* apply the unpickler operation to the input stream *)
    fun read (optModId, tyId) = if U.isBoxed tyId
	  then CL.mkApply (TyV.getName tyId ^ "::read", [isArg])
	  else CL.mkApply (baseReader(optModId, tyId), [isArg])

  (* return a list of delete statements for boxed fields of an object *)
    fun deleteStms obj = let
	  fun field label = CL.mkIndirect(CL.mkVar "this", U.fieldName label)
	  fun delete (label, E.OPTION(_, tyId), stms) =
		if U.isBoxed tyId
		  then CL.mkIfThen(
		    CL.mkBinOp(field label, CL.#!=, CL.mkVar "nullptr"),
		    CL.mkDelete(field label)) :: stms
		  else stms
	    | delete (_, E.SEQUENCE _, stms) = stms (* std::vector is unboxed *)
	    | delete (_, E.SHARED _, _) = raise Fail "shared types not supported"
	    | delete (label, E.BASE(_, tyId), stms) =
		if U.isBoxed tyId
		  then CL.mkDelete(field label) :: stms
		  else stms
	  in
	    case obj
	     of E.TUPLE fields => List.foldr
		  (fn ((i, ty), stms) => delete(AST.Pos i, ty, stms))
		    [] fields
	      | E.RECORD fields => List.foldr
		  (fn ((l, ty), stms) => delete(AST.Lab l, ty, stms))
		    [] fields
	    (* end case *)
	  end

    fun gen (AST.Module{isPrim=false, id, decls}) = let
	  val namespace = ModV.getName id
	  in
	    CL.D_Namespace(namespace, List.foldr genType [] (!decls))
	  end
      | gen _ = raise Fail "GenTypes.gen: unexpected primitive module"

    and genType (tyDcl as AST.TyDcl{def, ...}, dcls) = let
	  val (id, encoding) = E.encoding tyDcl
	  val name = TyV.getName id
	  in
	    case encoding
	     of E.UNIT conId => let
		(* a single-constructor enum requires no storage in the pickle *)
		  val ty = CL.T_Named name
		  val pickler = CL.mkFuncDcl (
			CL.voidTy, U.enumPickler name,
			[osParam, CL.param(ty, "v")],
			CL.mkBlock[])
		  val unpickler = CL.mkFuncDcl (
			ty, U.enumUnpickler name,
			[isParam],
			CL.mkReturn(SOME(CL.mkVar(U.enumConstrName conId))))
		  in
		    pickler :: unpickler :: dcls
		  end
	      | E.ENUM(nCons, cons) => let
		  val tagTyId = E.tagTyId nCons
		  val ty = CL.T_Named name
		  val pickler = CL.mkFuncDcl (
			CL.voidTy, U.enumPickler name,
			[osParam, CL.param(ty, "v")],
			write (
			  SOME PT.primTypesId,
			  tagTyId,
			  CL.mkStaticCast(CL.T_Named(TyV.getName tagTyId), CL.mkVar "v")))
		  val unpickler = CL.mkFuncDcl (
			ty, U.enumUnpickler name,
			[isParam],
			CL.mkReturn(SOME(
			  CL.mkStaticCast(ty, read (SOME PT.primTypesId, tagTyId)))))
		  in
		    pickler :: unpickler :: dcls
		  end
	      | E.WRAP(_, obj) => genProdMeths (id, name, obj) @ dcls
	      | E.SWITCH(attribs, nCons, cons) =>
		  genSumMeths (id, name, attribs, nCons, cons) @
		  genConsMeths (attribs, nCons, cons) @ dcls
	      | E.OBJ obj => genProdMeths (id, name, obj) @ dcls
	      | E.ALIAS ty => let
		  val cTy = CL.T_Named name
		  val vv = CL.mkVar "v"
		  val pickler = CL.mkFuncDcl (
			CL.voidTy, TyV.getWriter id,
			[osParam, CL.param(cTy, "v")],
			CL.mkBlock(encodeTy(vv, ty)))
		  val unpickler = let
			val stms = decodeTy ("v", ty)
			in
			  CL.mkFuncDcl (
			    cTy, TyV.getReader id,
			    [isParam],
			    CL.mkBlock(stms @ [CL.mkReturn(SOME vv)]))
			end
		  in
		    pickler :: unpickler :: dcls
		  end
	    (* end case *)
	  end

  (* generate the unpickling an destructor functions for a sum type *)
    and genSumMeths (tyId, name, optAttribs, nCons, cons) = let
	  val ty = CL.T_Named name
	  val ptrTy = CL.T_Ptr ty
	  val tagTy = CL.T_Named "_tag_t"
	(* read common attribute fields *)
	  val (getAttribs, attribArgs) = (case optAttribs
		 of NONE => ([], [])
		  | SOME obj => decodeFields obj
		(* end case *))
	(* unpickle a constructor *)
	  fun doCase (_, conId, fields) = let
		val label = [U.constrTagName conId]
		val (getFields, args) = (case fields
		       of NONE => ([], [])
			| SOME obj => decodeFields obj
		      (* end case *))
		val newExp = CL.mkNew(CL.T_Named(ConV.getName conId), attribArgs @ args)
		in
		  (label, [CL.mkBlock(getFields @ [CL.mkReturn(SOME newExp)])])
		end
	(* unpickler body *)
	  val body = [CL.mkSwitch(CL.mkVar "tag", List.map doCase cons)]
	  val body = getAttribs @ body
	  val body = (* get tag *)
		CL.mkDeclInit(tagTy, "tag",
		  CL.mkStaticCast(tagTy, read (SOME PT.primTypesId, E.tagTyId nCons))) :: body
	  val unpickler = CL.D_Func(
		[], ptrTy, [CL.SC_Type ty], "read", [isParam],
		SOME(CL.mkBlock body))
	(* destructor *)
	  val destr = CL.mkDestrDcl(name,
		case optAttribs
		 of NONE => CL.mkBlock[]
		  | SOME obj => CL.mkBlock(deleteStms obj))
	  in
	    [unpickler, destr]
	  end

  (* generate the pickler and destructor methods for a sum-type constructor *)
    and genConsMeths (attribs, ncons, cons) = let
	  val tagTyId = E.tagTyId ncons
	  fun doCons ((tag, conId, optArg), dcls) = let
		val name = ConV.getName conId
		val payload = E.prefixWithAttribs(attribs, optArg)
	      (* pickler *)
		val body = (case payload
		       of SOME obj => encodeFields obj
			| NONE => []
		      (* end case *))
		val body = write(
			SOME PT.primTypesId, tagTyId, CL.mkVar(U.constrTagName conId)
		      ) :: body
		val pickler = CL.D_Func(
		      [], CL.voidTy, [CL.SC_Type(CL.T_Named name)], "write", [osParam],
		      SOME(CL.mkBlock body))
	      (* destructor *)
		val destr = CL.mkDestrDcl(name,
		      case payload
		       of NONE => CL.mkBlock[]
			| SOME obj => CL.mkBlock(deleteStms obj))
		in
		  pickler :: destr :: dcls
		end
	  in
	    List.foldr doCons [] cons
	  end

  (* generate the methods for a product type *)
    and genProdMeths (id, name, obj) = let
	  val ty = CL.T_Named name
	  val ptrTy = CL.T_Ptr ty
	(* pickler *)
	  val pickler =
		CL.mkMethDcl(
		  name, CL.voidTy, "write", [osParam],
		  CL.mkBlock(encodeFields obj))
	(* unpickler *)
	  val unpickler = let
		val (getFields, args) = decodeFields obj
		val newExp = CL.mkNew(ty, args)
		val body = getFields @ [CL.mkReturn(SOME newExp)]
		in
		  CL.D_Func (
		    [], ptrTy, [CL.SC_Type ty], "read", [isParam],
		    SOME(CL.mkBlock body))
		end
	(* destructor *)
	  val destr = CL.mkDestrDcl(name, CL.mkBlock(deleteStms obj))
	  in
	    [pickler, unpickler, destr]
	  end

  (* generate the pickling functions for a type alias *)
    and genAliasFuncs _ = [] (* FIXME *)

  (* generate code for encoding the fields of a product *)
    and encodeFields (E.TUPLE fields) = let
	  fun enc ([], stms) = List.rev stms
	    | enc ((ix, ty)::flds, stms) = let
		val field = CL.mkIndirect(CL.mkVar "this", U.tupleFieldName ix)
		val stms = List.revAppend (encodeTy(field, ty), stms)
		in
		  enc (flds, stms)
		end
	  in
	    enc (fields, [])
	  end
      | encodeFields (E.RECORD fields) = let
	  fun enc ([], stms) = List.rev stms
	    | enc ((label, ty)::flds, stms) = let
		val field = CL.mkIndirect(CL.mkVar "this", U.recordFieldName label)
		val stms = List.revAppend (encodeTy(field, ty), stms)
		in
		  enc (flds, stms)
		end
	  in
	    enc (fields, [])
	  end

  (* generate code for pickling a type expression *)
    and encodeTy (arg, E.OPTION(optModId, tyId)) = [] (* FIXME *)
      | encodeTy (arg, E.SEQUENCE(optModId, tyId)) = [] (* FIXME *)
      | encodeTy (arg, E.SHARED _) = raise Fail "shared types not supported yet"
      | encodeTy (arg, E.BASE(optModId, tyId)) = [write (optModId, tyId, arg)]

  (* generate code for decoding the fields of a product *)
    and decodeFields (E.TUPLE fields) = let
	  fun dec ([], stms, args) = (List.rev stms, List.rev args)
	    | dec ((ix, ty)::flds, stms, args) = let
		val name = "f" ^ Int.toString ix
		val stms = List.revAppend (decodeTy(name, ty), stms)
		in
		  dec (flds, stms, CL.mkVar name :: args)
		end
	  in
	    dec (fields, [], [])
	  end
      | decodeFields (E.RECORD fields) = let
	  fun dec ([], stms, args) = (List.rev stms, List.rev args)
	    | dec ((label, ty)::flds, stms, args) = let
		val name = "f" ^ label
		val stms = List.revAppend (decodeTy(name, ty), stms)
		in
		  dec (flds, stms, CL.mkVar name :: args)
		end
	  in
	    dec (fields, [], [])
	  end

  (* generate code for decoding a type expression *)
    and decodeTy (x, E.OPTION ty) = [] (* FIXME *)
      | decodeTy (x, E.SEQUENCE ty) = [] (* FIXME *)
      | decodeTy (x, E.SHARED ty) = raise Fail "shared types not supported yet"
      | decodeTy (x, E.BASE ty) = [CL.mkDeclInit(CL.autoTy, x, read ty)]

  end


