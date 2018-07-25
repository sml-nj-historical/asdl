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
      | genSig _ = raise Fail "genSig: unexpected primitive module"

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
      | genStr _ = raise Fail "genStr: unexpected primitive module"

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
	  fun gen (arg, E.SWITCH rules) = S.caseExp(arg, List.map genRule rules)
	    | gen (arg, E.TUPLE tys) = let
		fun write (i, ty) = gen (S.selectExp(Int.toString i, arg), ty)
		in
		  S.SEQexp(List.mapi write tys)
		end
	    | gen (arg, E.RECORD fields) = let
		fun write (lab, ty) = gen (S.selectExp(lab, arg), ty)
		in
		  S.SEQexp(List.map write fields)
		end
	    | gen (arg, E.OPTION ty) =
		S.appExp(
		  funApp ("write_option", [S.IDexp(baseWriter ty)]),
		  pairExp(outSV, arg))
	    | gen (arg, E.SEQUENCE ty) =
		S.appExp(
		  funApp ("write_list", [S.IDexp(baseWriter ty)]),
		  pairExp(outSV, arg))
	    | gen (arg, E.SHARED ty) = raise Fail "shared types not supported yet"
	    | gen (arg, E.BASE ty) = funApp (baseWriter ty, [outSV, arg])
	  and genRule (tag, conId, optArg) = let
		val wrTag = funApp(
		      baseWriter(SOME PT.primTypesId, PT.uintTyId),
		      [outSV, S.NUMexp("0w" ^ Int.toString tag)])
		val conName = ConV.getName conId
		in
		  case optArg
		   of NONE => (S.IDpat conName, wrTag)
		    | SOME(E.TUPLE tys) => let
			val args = List.mapi (fn (i, _) => "x"^Int.toString i) tys
			val pat = S.CONpat(
			      conName,
			      S.tuplePat(List.map S.IDpat args))
			val exp = S.SEQexp(wrTag :: ListPair.map gen' (args, tys))
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
			val exp = S.SEQexp(wrTag :: List.map gen' flds)
			in
			  (pat, exp)
			end
		    | SOME ty => (S.CONpat(conName, S.IDpat "x"), S.SEQexp[wrTag, gen'("x", ty)])
		  (* end case *)
		end
	  and gen' (x, ty) = gen (S.IDexp x, ty)
	  in
	    S.simpleFB(wrName, ["buf", "obj"], gen(S.IDexp "obj", encoding))
	  end

    and genReader (rdName, encoding) = let
	  val inSV = S.IDexp "inS"
	  fun baseReader (NONE, tyId) = TyV.getReader tyId
	    | baseReader (SOME modId, tyId) = concat[
		  ModV.getIOName modId, ".", TyV.getReader tyId
		]
	  fun gen (E.SWITCH rules) = let
		val decodeTag = funApp(
		      baseReader(SOME PT.primTypesId, PT.uintTyId),
		      [inSV])
		val dfltRule = (S.WILDpat, S.raiseExp(S.IDexp "ASDL.DecodeError"))
		in
		  S.caseExp(decodeTag, List.map genRule rules @ [dfltRule])
		end
	    | gen (E.TUPLE tys) = genTuple (tys, fn x => x)
	    | gen (E.RECORD fields) = genRecord (fields, fn x => x)
	    | gen (E.OPTION ty) =
		S.appExp(
		  funApp ("readoption", [S.IDexp(baseReader ty)]),
		  inSV)
	    | gen (E.SEQUENCE ty) =
		S.appExp(
		  funApp ("read_list", [S.IDexp(baseReader ty)]),
		  inSV)
	    | gen (E.SHARED ty) = raise Fail "shared types not supported yet"
	    | gen (E.BASE ty) = funApp (baseReader ty, [inSV])
	  and genRule (tag, conId, optArg) = let
		val conName = ConV.getName conId
		val pat = S.NUMpat("0w"^Int.toString tag)
		in
		  case optArg
		   of NONE => (pat, S.IDexp conName)
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
		      (fn (x, ty) => S.VALdec(S.IDpat x, gen ty))
			(xs, tys)
		in
		  S.LETexp(decs, k(S.TUPLEexp(List.map S.IDexp xs)))
		end
	  and genRecord (fields, k) = let
		val decs = List.map
		      (fn (lab, ty) => S.VALdec(S.IDpat lab, gen ty))
			fields
		val fields = List.map (fn (lab, _) => (lab, S.IDexp lab)) fields
		in
		  S.LETexp(decs, k(S.RECORDexp fields))
		end
	  in
	    S.simpleFB(rdName, ["slice"], gen encoding)
	  end

  end
