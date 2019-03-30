(* gen-pickle.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the memory pickling code for the SML view
 *
 * TODO: merge with gen-io.sml
 *)

structure GenPickle : sig

  (* generate the pickler structure *)
    val gen : AST.module -> SML.top_decl

  end = struct

    structure PT = PrimTypes
    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure E = Encoding
    structure S = SML

  (***** Structure generation *****)

  (* generate a simple application *)
    fun funApp (f, args) = S.appExp(S.IDexp f, S.tupleExp args)
  (* pairs *)
    fun pairPat (a, b) = S.TUPLEpat[a, b]
    fun pairExp (a, b) = S.TUPLEexp[a, b]

    fun pklMod () = ModV.getPickleName PrimTypes.primTypesId
    fun outStrmTy () = S.CONty([], pklMod() ^ ".outstream")
    fun inStrmTy () = S.CONty([], pklMod() ^ ".instream")

    fun gen (AST.Module{isPrim=false, id, decls}) = let
	  val typeModName = ModV.getName id
	  val pickleModName = ModV.getPickleName id
	  val sign = S.AUGsig(
(* TODO: move Util.sigName to SML view *)
		S.IDsig(Util.sigName(ModV.getPickleSigName id, NONE)),
		[ S.WHERETY([], ["instream"], inStrmTy()),
		  S.WHERETY([], ["outstream"], outStrmTy())])
	  fun genGrp (dcls, dcls') = S.FUNdec(List.foldr (genType typeModName) [] dcls) :: dcls'
	  val decls = List.foldr genGrp [] (SortDecls.sort (!decls))
	  val decls = S.VERBdec[
		  StringSubst.expand [("PICKLER", pklMod ())] Fragments.pickleUtil
		] :: decls
	  in
	    S.STRtop(pickleModName, SOME(false, sign), S.BASEstr decls)
	  end
      | gen _ = raise Fail "GenPickle.gen: unexpected primitive module"

    and genType typeModName (dcl, fbs) = let
	  val (id, encoding) = E.encoding dcl
	  val name = TyV.getName id
	  in
	    genEncoder (typeModName, TyV.getEncoder id, encoding) ::
	    genDecoder (typeModName, TyV.getDecoder id, encoding) ::
	      fbs
	  end

    and genEncoder (typeModName, encName, encoding) = let
	  val bufV = S.IDexp "buf"
	  fun getConName conId = concat[typeModName, ".", ConV.getName conId]
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
		fun genRule (tag, conId) = (S.IDpat(getConName conId), genTag (tagTyId, tag))
		in
		  S.caseExp(arg, List.map genRule cons)
		end
	    | gen (arg, E.WRAP(conId, fields)) = let
		val (lhsPat, xs) = objPat fields
		in
		  S.LETexp(
		    [S.VALdec(S.CONpat(getConName conId, lhsPat), arg)],
		    S.SEQexp(List.map genTy' xs))
		end
	    | gen (arg, E.SWITCH(optAttribs, nCons, cons)) = let
		val tagTyId = E.tagTyId nCons
		fun genRule (tag, conId, optArg) = let
		      val encTag = genTag(tagTyId, tag)
		      val conName = getConName conId
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
	  and genProd (arg, obj) = let
		val (pat, args) = objPat obj
		in
		  S.LETexp(
		    [S.VALdec(pat, arg)],
		    S.SEQexp(List.map genTy' args))
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
		  funApp ("writeOption", [S.IDexp(baseEncode ty)]),
		  pairExp(bufV, arg))
	    | genTy (arg, E.SEQUENCE ty) =
		S.appExp(
		  funApp ("writeSeq", [S.IDexp(baseEncode ty)]),
		  pairExp(bufV, arg))
	    | genTy (arg, E.SHARED ty) = raise Fail "shared types not supported yet"
	    | genTy (arg, E.BASE ty) = funApp (baseEncode ty, [bufV, arg])
	  and genTy' (x, ty) = genTy (S.IDexp x, ty)
	  in
	    S.simpleFB(encName, ["buf", "obj"], gen(S.IDexp "obj", encoding))
	  end

    and genDecoder (typeModName, decName, encoding) = let
	  val inSV = S.IDexp "inS"
	  fun getConName conId = concat[typeModName, ".", ConV.getName conId]
	  fun baseDecode (NONE, tyId) = TyV.getDecoder tyId
	    | baseDecode (SOME modId, tyId) = concat[
		  ModV.getPickleName modId, ".", TyV.getDecoder tyId
		]
	  fun genTag tagTyId = funApp(
		baseDecode(SOME PT.primTypesId, tagTyId),
		[inSV])
	  val dfltRule = (S.WILDpat, S.raiseExp(S.IDexp "ASDL.DecodeError"))
	  fun gen (E.UNIT conId) = S.IDexp(getConName conId)
	    | gen (E.ENUM(nCons, cons)) = let
		fun genRule (tag, conId) = let
		      val pat = S.NUMpat("0w"^Int.toString tag)
		      in
			(pat, S.IDexp(getConName conId))
		      end
		in
		  S.caseExp(genTag(E.tagTyId nCons), List.map genRule cons @ [dfltRule])
		end
	    | gen (E.WRAP(conId, fields)) =
		genObj(fields, fn  x => S.appExp(S.IDexp(getConName conId), x))
	    | gen (E.SWITCH(optAttribs, nCons, cons)) = let
		fun genRule (tag, conId, optArg) = let
		      val conName = getConName conId
		      val pat = S.NUMpat("0w"^Int.toString tag)
		      in
			case E.prefixWithAttribs(optAttribs, optArg)
			 of NONE => (pat, S.IDexp conName)
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
		      (fn (x, (_, ty)) => S.VALdec(S.IDpat x, genTy ty))
			(xs, tys)
		in
		  S.LETexp(decs, k (S.TUPLEexp(List.map S.IDexp xs)))
		end
	    | genObj (E.RECORD fields, k) = let
		val decs = List.map
		      (fn (lab, ty) => S.VALdec(S.IDpat lab, genTy ty))
			fields
		val fields = List.map (fn (lab, _) => (lab, S.IDexp lab)) fields
		in
		  S.LETexp(decs, k (S.RECORDexp fields))
		end
	  and genTy (E.OPTION ty) =
		S.appExp(
		  funApp ("readOption", [S.IDexp(baseDecode ty)]),
		  inSV)
	    | genTy (E.SEQUENCE ty) =
		S.appExp(
		  funApp ("readSeq", [S.IDexp(baseDecode ty)]),
		  inSV)
	    | genTy (E.SHARED ty) = raise Fail "shared types not supported yet"
	    | genTy (E.BASE ty) = funApp (baseDecode ty, [inSV])
	  in
	    S.simpleFB(decName, ["inS"], gen encoding)
	  end

  end
