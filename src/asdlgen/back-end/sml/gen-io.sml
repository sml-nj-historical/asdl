(* gen-io.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure GenIO : sig

  (* generate the signature for the pickle-io structure *)
    val genSig : AST.module -> SML.top_decl

  (* generate the pickle-io structure *)
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
	  val sigName = Util.sigName(ModV.getIOName id, NONE)
	  val specs = List.foldr (genSpec typeModName) [] (!decls)
	  in
	    S.SIGtop(sigName, S.BASEsig specs)
	  end
      | genSig _ = raise Fail "GenIO.genSig: unexpected primitive module"

  (* generate the encoder/decoder specifications for a type *)
    and genSpec modName (AST.TyDcl{id, ...}, specs) = let
	  val ty = S.CONty([], TyV.getName id)
	  val outStrmTy = S.CONty([], "BinIO.outstream")
	  val inStrmTy = S.CONty([], "BinIO.instream")
	  val unitTy = S.CONty([], "unit")
	(* writer *)
	  val wrTy = S.FUNty(S.TUPLEty[outStrmTy, ty], unitTy)
	  val wrSpc = S.VALspec(TyV.getWriter id, wrTy)
	(* reader *)
	  val rdTy = S.FUNty(inStrmTy, ty)
	  val rdSpc = S.VALspec(TyV.getReader id, rdTy)
	  in
	    wrSpc :: rdSpc :: specs
	  end

  (***** Structure generation *****)

  (* generate a simple application *)
    fun funApp (f, args) = S.appExp(S.IDexp f, S.tupleExp args)
  (* pairs *)
    fun pairPat (a, b) = S.TUPLEpat[a, b]
    fun pairExp (a, b) = S.TUPLEexp[a, b]

    fun genStr (AST.Module{isPrim=false, id, decls}) = let
	  val typeModName = ModV.getName id
	  val ioModName = ModV.getIOName id
	  val sigName = Util.sigName(ioModName, NONE)
	  fun genGrp (dcls, dcls') = S.FUNdec(List.foldr genType [] dcls) :: dcls'
	  val decls = List.foldr genGrp [] (SortDecls.sort (!decls))
	  val decls = S.VERBdec[Fragments.ioUtil] :: decls
	  in
	    S.STRtop(ioModName, SOME(false, S.IDsig sigName), S.BASEstr decls)
	  end
      | genStr _ = raise Fail "GenIO.genStr: unexpected primitive module"

    and genType (dcl, fbs) = let
	  val (id, encoding) = E.encoding dcl
	  val name = TyV.getName id
	  in
	    genWriter (TyV.getWriter id, encoding) ::
	    genReader (TyV.getReader id, encoding) ::
	      fbs
	  end

    and genWriter (wrName, encoding) = let
	  val outSV = S.IDexp "outS"
	  fun baseWriter (NONE, tyId) = TyV.getWriter tyId
	    | baseWriter (SOME modId, tyId) = concat[
		  ModV.getIOName modId, ".", TyV.getWriter tyId
		]
	  fun genTag (tagTyId, tag) = funApp(
		baseWriter(SOME PT.primTypesId, tagTyId),
		[outSV, S.NUMexp("0w" ^ Int.toString tag)])
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
		      val wrTag = genTag(tagTyId, tag)
		      val conName = ConV.getName conId
		      in
			case E.prefixWithAttribs(optAttribs, optArg)
			 of NONE => (S.IDpat conName, wrTag)
			  | SOME obj => let
			      val (pat, flds) = objPat obj
			      val pat = S.CONpat(conName, pat)
			      val exp = S.SEQexp(wrTag :: List.map genTy' flds)
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
		fun write (i, ty) = genTy (S.selectExp(Int.toString i, arg), ty)
		in
		  S.SEQexp(List.map write tys)
		end
	    | genProd (arg, E.RECORD fields) = let
		fun write (lab, ty) = genTy (S.selectExp(lab, arg), ty)
		in
		  S.SEQexp(List.map write fields)
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
		  funApp ("write_option", [S.IDexp(baseWriter ty)]),
		  pairExp(outSV, arg))
	    | genTy (arg, E.SEQUENCE ty) =
		S.appExp(
		  funApp ("write_list", [S.IDexp(baseWriter ty)]),
		  pairExp(outSV, arg))
	    | genTy (arg, E.SHARED ty) = raise Fail "shared types not supported yet"
	    | genTy (arg, E.BASE ty) = funApp (baseWriter ty, [outSV, arg])
	  and genTy' (x, ty) = genTy (S.IDexp x, ty)
	  in
	    S.simpleFB(wrName, ["buf", "obj"], gen(S.IDexp "obj", encoding))
	  end

    and genReader (rdName, encoding) = let
	  val inSV = S.IDexp "inS"
	  fun baseReader (NONE, tyId) = TyV.getReader tyId
	    | baseReader (SOME modId, tyId) = concat[
		  ModV.getIOName modId, ".", TyV.getReader tyId
		]
	  fun genTag tagTyId = funApp(
		baseReader(SOME PT.primTypesId, tagTyId),
		[inSV])
	  val dfltRule = (S.WILDpat, S.raiseExp(S.IDexp "ASDL.DecodeError"))
	  fun gen (E.UNIT conId) = S.IDexp(ConV.getName conId)
	    | gen (E.ENUM(nCons, cons)) = let
		fun genRule (tag, conId) = let
		      val pat = S.NUMpat("0w"^Int.toString tag)
		      in
			(pat, S.IDexp(ConV.getName conId))
		      end
		in
		  S.caseExp(genTag(E.tagTyId nCons), List.map genRule cons @ [dfltRule])
		end
	    | gen (E.WRAP(conId, fields)) =
		genObj(fields, fn  x => S.appExp(S.IDexp(ConV.getName conId), x))
	    | gen (E.SWITCH(optAttribs, nCons, cons)) = let
		fun genRule (tag, conId, optArg) = let
		      val conName = ConV.getName conId
		      val pat = S.NUMpat("0w"^Int.toString tag)
		      in
			case E.prefixWithAttribs(optAttribs, optArg)
			 of NONE => (pat, S.IDexp conName)
			  | SOME obj => (pat, genObj(obj, fn x => S.appExp(S.IDexp conName, x)))
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
		  S.LETexp(decs, k(S.TUPLEexp(List.map S.IDexp xs)))
		end
	    | genObj (E.RECORD fields, k) = let
		val decs = List.map
		      (fn (lab, ty) => S.VALdec(S.IDpat lab, genTy ty))
			fields
		val fields = List.map (fn (lab, _) => (lab, S.IDexp lab)) fields
		in
		  S.LETexp(decs, k(S.RECORDexp fields))
		end
	  and genTy (E.OPTION ty) =
		S.appExp(
		  funApp ("read_option", [S.IDexp(baseReader ty)]),
		  inSV)
	    | genTy (E.SEQUENCE ty) =
		S.appExp(
		  funApp ("read_list", [S.IDexp(baseReader ty)]),
		  inSV)
	    | genTy (E.SHARED ty) = raise Fail "shared types not supported yet"
	    | genTy (E.BASE ty) = funApp (baseReader ty, [inSV])
	  in
	    S.simpleFB(rdName, ["inS"], gen encoding)
	  end

  end
