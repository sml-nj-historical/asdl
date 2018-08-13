(* gen-pickle.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the pickling code for the SML view
 *)

structure GenPickle : sig

  (* generate the signature for the pickler structure *)
    val genSig : AST.module -> SML.top_decl

  (* generate the pickler structure *)
    val genStr : AST.module -> SML.top_decl

  end = struct

    structure PT = PrimTypes
    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure E = Encoding
    structure S = SML

  (***** Signature generation *****)

    fun genSig (AST.Module{isPrim=false, id, decls}) = let
	  val typeModName = ModV.getName id
	  val sigName = Util.sigName(ModV.getPickleName id, NONE)
	  val specs = List.foldr (genSpec typeModName) [] (!decls)
	  in
	    S.SIGtop(sigName, S.BASEsig specs)
	  end
      | genSig _ = raise Fail "GenPickle.genSig: unexpected primitive module"

  (* generate the encoder/decoder specifications for a type *)
    and genSpec modName (AST.TyDcl{id, ...}, specs) = let
	  val ty = S.CONty([], TyV.getName id)
	  val bufTy = S.CONty([], "Word8Buffer.buf")
	  val vecTy = S.CONty([], "Word8Vector.vector")
	  val sliceTy = S.CONty([], "Word8VectorSlice.slice")
	  val unitTy = S.CONty([], "unit")
	(* encoder *)
	  val encTy = S.FUNty(S.TUPLEty[bufTy, ty], unitTy)
	  val encSpc = S.VALspec(TyV.getEncoder id, encTy)
	(* decoder *)
	  val decTy = S.FUNty(sliceTy, S.TUPLEty[ty, sliceTy])
	  val decSpc = S.VALspec(TyV.getDecoder id, decTy)
	  in
	    encSpc :: decSpc :: specs
	  end

  (***** Structure generation *****)

  (* generate a simple application *)
    fun funApp (f, args) = S.appExp(S.IDexp f, S.tupleExp args)
  (* pairs *)
    fun pairPat (a, b) = S.TUPLEpat[a, b]
    fun pairExp (a, b) = S.TUPLEexp[a, b]

    fun genStr (AST.Module{isPrim=false, id, decls}) = let
	  val typeModName = ModV.getName id
	  val pickleModName = ModV.getPickleName id
	  val sigName = Util.sigName(pickleModName, NONE)
	  fun genGrp (dcls, dcls') = S.FUNdec(List.foldr genType [] dcls) :: dcls'
	  val decls = List.foldr genGrp [] (SortDecls.sort (!decls))
	  val decls = S.VERBdec[Fragments.pickleUtil] :: decls
	  in
	    S.STRtop(pickleModName, SOME(false, S.IDsig sigName), S.BASEstr decls)
	  end
      | genStr _ = raise Fail "GenPickle.genStr: unexpected primitive module"

    and genType (dcl, fbs) = let
	  val (id, encoding) = E.encoding dcl
	  val name = TyV.getName id
	  in
	    genEncoder (TyV.getEncoder id, encoding) ::
	    genDecoder (TyV.getDecoder id, encoding) ::
	      fbs
	  end

    and genEncoder (encName, encoding) = let
	  val bufV = S.IDexp "buf"
	  fun baseEncode (NONE, tyId) = TyV.getEncoder tyId
	    | baseEncode (SOME modId, tyId) = concat[
		  ModV.getPickleName modId, ".", TyV.getEncoder tyId
		]
	  fun genTag (tagTyId, tag) = funApp(
		baseEncode(SOME PT.primTypesId, tagTyId),
		[bufV, S.NUMexp("0w" ^ Int.toString tag)])
	  fun gen (arg, E.UNIT conId) = S.unitExp (* no storage required *)
	    | gen (arg, E.ENUM(nCons, cons)) = let
		val tagTyId = E.tagTyId nCons
		fun genRule (tag, conId) = (S.IDpat(ConV.getName conId), genTag (tagTyId, tag))
		in
		  S.caseExp(arg, List.map genRule cons)
		end
	    | gen (arg, E.WRAP(conId, fields)) = let
		val (lhsPat, xs) = objPat fields
		in
		  S.LETexp(
		    [S.VALdec(S.CONpat(ConV.getName conId, lhsPat), arg)],
		    S.SEQexp(List.map genTy' xs))
		end
	    | gen (arg, E.SWITCH(optAttribs, nCons, cons)) = let
		val tagTyId = E.tagTyId nCons
		fun genRule (tag, conId, optArg) = let
		      val encTag = genTag(tagTyId, tag)
		      val conName = ConV.getName conId
		      in
			case E.prefixWithAttribs(optAttribs, optArg)
			 of NONE => (S.IDpat conName, encTag)
			  | SOME obj => let
			      val (pat, flds) = objPat obj
			      val pat = S.CONpat(conName, pat)
			      val exp = S.SEQexp(encTag :: List.map genTy' flds)
			      in
				(pat, exp)
			      end
			(* end case *)
		      end
		in
		  S.caseExp(arg, List.map genRule cons)
		end
	    | gen (arg, E.OBJ obj) = genProd (arg, obj)
	    | gen (arg, E.ALIAS ty) = genTy (arg, ty)
	  and genProd (arg, E.TUPLE tys) = let
		fun encode (i, ty) = genTy (S.selectExp(Int.toString i, arg), ty)
		in
		  S.SEQexp(List.map encode tys)
		end
	    | genProd (arg, E.RECORD fields) = let
		fun encode (lab, ty) = genTy (S.selectExp(lab, arg), ty)
		in
		  S.SEQexp(List.map encode fields)
		end
	(* create a pattern for matching against a product type; returns the pattern and the
	 * bound variables with their types.
	 *)
	  and objPat (E.TUPLE tys) = let
		val args = List.map (fn (i, ty) => ("x"^Int.toString i, ty)) tys
		in
		  (S.tuplePat(List.map (S.IDpat o #1) args), args)
		end
	    | objPat (E.RECORD flds) = let
		val pat = S.RECORDpat{
			fields = List.map (fn fld => (#1 fld, S.IDpat(#1 fld))) flds,
			flex = false
		      }
		in
		  (pat, flds)
		end
	  and genTy (arg, E.OPTION ty) =
		S.appExp(
		  funApp ("encode_option", [S.IDexp(baseEncode ty)]),
		  pairExp(bufV, arg))
	    | genTy (arg, E.SEQUENCE ty) =
		S.appExp(
		  funApp ("encode_list", [S.IDexp(baseEncode ty)]),
		  pairExp(bufV, arg))
	    | genTy (arg, E.SHARED ty) = raise Fail "shared types not supported yet"
	    | genTy (arg, E.BASE ty) = funApp (baseEncode ty, [bufV, arg])
	  and genTy' (x, ty) = genTy (S.IDexp x, ty)
	  in
	    S.simpleFB(encName, ["buf", "obj"], gen(S.IDexp "obj", encoding))
	  end

    and genDecoder (decName, encoding) = let
	  val sliceP = S.IDpat "slice"
	  val sliceV = S.IDexp "slice"
	  fun baseDecode (NONE, tyId) = TyV.getDecoder tyId
	    | baseDecode (SOME modId, tyId) = concat[
		  ModV.getPickleName modId, ".", TyV.getDecoder tyId
		]
	  fun genTag tagTyId = funApp(
		baseDecode(SOME PT.primTypesId, tagTyId),
		[sliceV])
	  val dfltRule = (S.WILDpat, S.raiseExp(S.IDexp "ASDL.DecodeError"))
	  fun gen (E.UNIT conId) = pairExp(S.IDexp(ConV.getName conId), sliceV)
	    | gen (E.ENUM(nCons, cons)) = let
		fun genRule (tag, conId) = let
		      val pat = pairPat(S.NUMpat("0w"^Int.toString tag), sliceP)
		      in
			(pat, pairExp(S.IDexp(ConV.getName conId), sliceV))
		      end
		in
		  S.caseExp(genTag(E.tagTyId nCons), List.map genRule cons @ [dfltRule])
		end
	    | gen (E.WRAP(conId, fields)) =
		genObj(fields, fn  x => S.appExp(S.IDexp(ConV.getName conId), x))
	    | gen (E.SWITCH(optAttribs, nCons, cons)) = let
		fun genRule (tag, conId, optArg) = let
		      val conName = ConV.getName conId
		      val pat = pairPat(S.NUMpat("0w"^Int.toString tag), sliceP)
		      in
			case E.prefixWithAttribs(optAttribs, optArg)
			 of NONE => (pat, pairExp(S.IDexp conName, sliceV))
			  | SOME obj =>
			      (pat, genObj(obj, fn x => S.appExp(S.IDexp conName, x)))
			(* end case *)
		      end
		in
		  S.caseExp(genTag(E.tagTyId nCons), List.map genRule cons @ [dfltRule])
		end
	    | gen (E.OBJ obj) = genObj(obj, fn x => x)
	    | gen (E.ALIAS ty) = genTy ty
	  and genObj (E.TUPLE tys, k) = let
		val xs = List.mapi (fn (i, _) => "x"^Int.toString i) tys
		val decs = ListPair.map
		      (fn (x, (_, ty)) => S.VALdec(pairPat(S.IDpat x, sliceP), genTy ty))
			(xs, tys)
		in
		  S.LETexp(decs, pairExp (k(S.TUPLEexp(List.map S.IDexp xs)), sliceV))
		end
	    | genObj (E.RECORD fields, k) = let
		val decs = List.map
		      (fn (lab, ty) => S.VALdec(pairPat(S.IDpat lab, sliceP), genTy ty))
			fields
		val fields = List.map (fn (lab, _) => (lab, S.IDexp lab)) fields
		in
		  S.LETexp(decs, pairExp (k(S.RECORDexp fields), sliceV))
		end
	  and genTy (E.OPTION ty) =
		S.appExp(
		  funApp ("decode_option", [S.IDexp(baseDecode ty)]),
		  sliceV)
	    | genTy (E.SEQUENCE ty) =
		S.appExp(
		  funApp ("decode_list", [S.IDexp(baseDecode ty)]),
		  sliceV)
	    | genTy (E.SHARED ty) = raise Fail "shared types not supported yet"
	    | genTy (E.BASE ty) = funApp (baseDecode ty, [sliceV])
	  in
	    S.simpleFB(decName, ["slice"], gen encoding)
	  end

  end
