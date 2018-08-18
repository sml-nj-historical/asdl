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
 *	void encode (asdl::outstream &os);
 *
 * and the decoder is a static class method with the following signature:
 *
 *	TYPE *decode (asdl::instream &is);
 *
 * For enumeration and alias types, we generate the functions:
 *
 *	void encode_TYPE (asdl::outstream &os, TYPE v);
 *	TYPE decode_TYPE (asdl::instream &is);
 *
 * TODO:
 * 	encode/decoder names should be taken from view
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
    fun baseEncode (NONE, tyId) = TyV.getEncoder tyId
      | baseEncode (SOME modId, tyId) = concat[
	    ModV.getName modId, "::", TyV.getEncoder tyId
	  ]

  (* unpickler function name for primitive types *)
    fun baseDecode (NONE, tyId) = TyV.getDecoder tyId
      | baseDecode (SOME modId, tyId) = concat[
	    ModV.getName modId, "::", TyV.getDecoder tyId
	  ]

  (* invoke the pickler operation on the argument *)
    fun encode (optModId, tyId, arg) = if U.isBoxed tyId
	  then CL.mkExpStm(CL.mkDispatch (arg, "encode", [osArg]))
	  else CL.mkCall (baseEncode(optModId, tyId), [osArg, arg])

  (* apply the unpickler operation to the input stream *)
    fun decode (optModId, tyId) = if U.isBoxed tyId
	  then CL.mkApply (TyV.getName tyId ^ "::decode", [isArg])
	  else CL.mkApply (baseDecode(optModId, tyId), [isArg])

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
			encode (
			  SOME PT.primTypesId,
			  tagTyId,
			  CL.mkStaticCast(CL.T_Named(TyV.getName tagTyId), CL.mkVar "v")))
		  val unpickler = CL.mkFuncDcl (
			ty, U.enumUnpickler name,
			[isParam],
			CL.mkReturn(SOME(
			  CL.mkStaticCast(ty, decode (SOME PT.primTypesId, tagTyId)))))
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
			CL.voidTy, TyV.getEncoder id,
			[osParam, CL.param(cTy, "v")],
			CL.mkBlock(encodeTy(vv, ty)))
		  val unpickler = let
			val stms = decodeTy ("v", ty)
			in
			  CL.mkFuncDcl (
			    cTy, TyV.getDecoder id,
			    [isParam],
			    CL.mkBlock(stms @ [CL.mkReturn(SOME vv)]))
			end
		  in
		    pickler :: unpickler :: dcls
		  end
	    (* end case *)
	  end

  (* generate the unpickling function for a sum type *)
    and genSumMeths (tyId, name, optAttribs, nCons, cons) = let
	  val ty = CL.T_Named name
	  val ptrTy = CL.T_Ptr ty
	  val tagTy = CL.T_Named "_tag_t"
	(* decode common attribute fields *)
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
		  CL.mkStaticCast(tagTy, decode (SOME PT.primTypesId, E.tagTyId nCons))) :: body
	  val unpickler = CL.D_Func (
		[], ptrTy, [CL.SC_Type ty], "decode", [isParam],
		SOME(CL.mkBlock body))
	  in
	    [unpickler]
	  end

  (* generate the destructor and pickler methods for a sum-type constructor *)
    and genConsMeths _ = [] (* FIXME *)

  (* generate the methods for a product type *)
    and genProdMeths _ = [] (* FIXME *)

  (* generate the pickling functions for a type alias *)
    and genAliasFuncs _ = [] (* FIXME *)

  (* generate code for decoding fields *)
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
      | decodeTy (x, E.BASE ty) = [CL.mkDeclInit(CL.autoTy, x, decode ty)]

  (* generate code for pickling a type expression *)
    and encodeTy (arg, E.OPTION(optModId, tyId)) = [] (* FIXME *)
      | encodeTy (arg, E.SEQUENCE(optModId, tyId)) = [] (* FIXME *)
      | encodeTy (arg, E.SHARED _) = raise Fail "shared types not supported yet"
      | encodeTy (arg, E.BASE(optModId, tyId)) = [encode (optModId, tyId, arg)]

  end


