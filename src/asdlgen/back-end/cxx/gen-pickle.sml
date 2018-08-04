(* gen-pickle.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the encoder/decoder methods for the typed of an ASDL module.  For an ASDL
 * type "TYPE", the encoder is a method with the following signature:
 *
 *	void encode (asdl::outstream &os);
 *
 * and the decoder is a constructor with the following signature:
 *
 *	TYPE (asdl::instream &os);
 *
 * For enumeration types, we generate functions:
 *
 *	void encode_TYPE (asdl::outstream &os, TYPE const &v);
 *	TYPE decode_TYPE (asdl::instream &os);
 *)

structure GenPickle : sig

  (* generate the implementation of the pickler functions.  The result will be
   * a namespace declaration enclosing the function definitions.
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

    val instrm = CL.T_Named "asdl::instream"
    val instrmRef = CL.T_Ref(CL.T_Named "asdl::instream")
    val outstrm = CL.T_Named "asdl::outstream"
    val outstrmRef = CL.T_Ref(CL.T_Named "asdl::outstream")

    fun gen (AST.Module{isPrim=false, id, decls}) = let
	  val namespace = ModV.getName id
	  in
	    CL.D_Namespace(namespace, List.foldr genType [] decls)
	  end
      | gen _ = raise Fail "genDcls: unexpected primitive module"

    and genType (dcl, dcls) = let
	  val (id, encoding) = E.encoding dcl
	  val name = TyV.getName id
	  in
	    genEncoder (id, encoding) ::
	    genDecoder (id, encoding) ::
	      dcls
	  end

    and genEncoder (tyId, encoding) = let
	  val encName = TyV.getEncoder tyId
	  val objTy = CL.T_Named(TyV.getName id)
	  val osV = CL.mkVar "os"
	  fun baseEncode (NONE, tyId) = TyV.getEncoder tyId
	    | baseEncode (SOME modId, tyId) = concat[
		  ModV.getName modId, "::", TyV.getEncoder tyId
		]
	  fun genStm arg = CL.mkBlock(gen arg)
	  and gen (arg, E.SWITCH rules) = let
	      (* determine the "type" of the tag *)
		val tagTyId = if (ncons <= 256) then PT.tag8TyId
		      then if (ncons <= 65536) then PT.tag16TyId
		      else raise Fail "too many constructors"
		in
		  if Util.isEnum tyId
		    then [S.mkCall(baseEncode (SOME PT.primTypesId, tagTyId), [arg])]
		    else [
			S.mkCall(baseEncode (SOME PT.primTypesId, tagTyId), [arg]),
			CL.mkSwitch(CL.mkIndirect(arg, "_tag"), List.map genCase rules)
		      ]
		end
	    | gen (arg, E.TUPLE tys) = let
		fun encode (i, ty) = gen (CL.mkIndirect(arg, Util.posName lab), ty)
		in
		  List.mapi encode tys
		end
	    | gen (arg, E.RECORD fields) = let
		fun encode (lab, ty) = gen (CL.mkIndirect(arg, Util.fieldName lab), ty)
		in
		  List.map encode fields
		end
	    | gen (arg, E.OPTION ty) =
		CL.mkIf(
		  CL.mkBinOp(CL.mkVar "nullptr", CL.#==, arg),
		  CL.mkCall("asdl::encode_tag8", [osV, CL.mkInt 0]),
		  if Util.isBoxed(#2 ty)
		    then genStm (arg, ty)
		    else genStm (CL.mkIndirectDispatch(arg, "value", [])))
	    | gen (arg, E.SEQUENCE ty) = [
		  CL.mkCall("asdl::encode_uint", [osV, CL.mkIndirectDispatch(arg, "size", [])]),
		  CL.mkFor(
		    CL.autoTy, ["it", CL.mkIndirectDispatch(arg, "cbegin", [])],
		    CL.mkBinOp(CL.mkVar "it", CL.#!=, CL.mkIndirectDispatch(arg, "cend", [])),
		    CL.mkUnOp(CL.%++, CL.mkVar "it"),
		    genStm (CL.mkUnOp(CL.%*, CL.mkVar "it"), ty))
		]
	    | gen (arg, E.SHARED ty) = raise Fail "shared types not supported yet"
	    | gen (arg, E.BASE ty) = funApp (baseEncode ty, [bufV, arg])
	  and genRule (tag, conId, optArg) = let
		val encTag = funApp(
		      baseEncode(SOME PT.primTypesId, PT.uintTyId),
		      [bufV, S.NUMexp("0w" ^ Int.toString tag)])
		val conName = ConV.getName conId
		in
		  case optArg
		   of NONE => (S.IDpat conName, encTag)
		    | SOME(E.TUPLE tys) => let
			val args = List.mapi (fn (i, _) => "x"^Int.toString i) tys
			val pat = S.CONpat(
			      conName,
			      S.tuplePat(List.map S.IDpat args))
			val exp = S.SEQexp(encTag :: ListPair.map gen' (args, tys))
			in
			  (pat, exp)
			end
		    | SOME(E.RECORD flds) => let
			val pat = S.CONpat(
			      conName,
			      S.RECORDpat{
				  fields = List.map (fn fld => (#1 fld, S.IDpat(#1 fld))) flds,
				  flex = false
				})
			val exp = S.SEQexp(encTag :: List.map gen' flds)
			in
			  (pat, exp)
			end
		    | SOME ty => (S.CONpat(conName, S.IDpat "x"), S.SEQexp[encTag, gen'("x", ty)])
		  (* end case *)
		end
	  and gen' (x, ty) = gen (S.IDexp x, ty)
	  in
	    if Util.isEnum tyId
	      then CL.mkFuncDcl(
		CL.voidTy, encName,
		[osParam, CL.PARAM([], constRefTyf(CL.T_Named(TyV.getName id)), "v")],
		gen' (mkVar "v"))
	      else CL.mkMethDcl(TyV.getName id, CL.voidTy, encName, [osParam], body)
	  end

    and genDecoder (tyId, encoding) = let
	  val decName = TyV.getDecoder tyId
	  val sliceP = S.IDpat "slice"
	  val sliceV = S.IDexp "slice"
	  fun baseDecode (NONE, tyId) = TyV.getDecoder tyId
	    | baseDecode (SOME modId, tyId) = concat[
		  ModV.getPickleName modId, ".", TyV.getDecoder tyId
		]
	  fun gen (E.SWITCH rules) = let
		val decodeTag = funApp(
		      baseDecode(SOME PT.primTypesId, PT.uintTyId),
		      [sliceV])
		val dfltRule = (S.WILDpat, S.raiseExp(S.IDexp "ASDL.DecodeError"))
		in
		  S.caseExp(decodeTag, List.map genRule rules @ [dfltRule])
		end
	    | gen (E.TUPLE tys) = genTuple (tys, fn x => x)
	    | gen (E.RECORD fields) = genRecord (fields, fn x => x)
	    | gen (E.OPTION ty) =
		S.appExp(
		  funApp ("decode_option", [S.IDexp(baseDecode ty)]),
		  sliceV)
	    | gen (E.SEQUENCE ty) =
		S.appExp(
		  funApp ("decode_list", [S.IDexp(baseDecode ty)]),
		  sliceV)
	    | gen (E.SHARED ty) = raise Fail "shared types not supported yet"
	    | gen (E.BASE ty) = funApp (baseDecode ty, [sliceV])
	  and genRule (tag, conId, optArg) = let
		val conName = ConV.getName conId
		val pat = pairPat(S.NUMpat("0w"^Int.toString tag), sliceP)
		in
		  case optArg
		   of NONE => (pat, pairExp(S.IDexp conName, sliceV))
		    | SOME(E.TUPLE tys) =>
			(pat, genTuple(tys, fn x => S.appExp(S.IDexp conName, x)))
		    | SOME(E.RECORD fields) =>
			(pat, genRecord(fields, fn x => S.appExp(S.IDexp conName, x)))
		    | SOME ty => (pat, gen ty)
		  (* end case *)
		end
	  and genTuple (tys, k) = let
		val xs = List.mapi (fn (i, _) => "x"^Int.toString i) tys
		val decs = ListPair.map
		      (fn (x, ty) => S.VALdec(pairPat(S.IDpat x, sliceP), gen ty))
			(xs, tys)
		in
		  S.LETexp(decs, pairExp (k(S.TUPLEexp(List.map S.IDexp xs)), sliceV))
		end
	  and genRecord (fields, k) = let
		val decs = List.map
		      (fn (lab, ty) => S.VALdec(pairPat(S.IDpat lab, sliceP), gen ty))
			fields
		val fields = List.map (fn (lab, _) => (lab, S.IDexp lab)) fields
		in
		  S.LETexp(decs, pairExp (k(S.RECORDexp fields), sliceV))
		end
	  in
	    S.simpleFB(decName, ["slice"], gen encoding)
	  end

  end


