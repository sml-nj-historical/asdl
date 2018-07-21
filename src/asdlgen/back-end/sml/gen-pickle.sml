(* gen-pickle.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure GenPickle : sig

  (* generate the signature for the pickler structure *)
    val genSig : AST.module -> SML.top_decl

  (* generate the pickler structure *)
    val genStr : AST.module -> SML.top_decl

  end = struct

    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure S = SML

  (***** Signature generation *****)

    fun genSig (AST.Module{isPrim=false, id, decls}) = let
	  val baseModName = ModV.getName id
	  val sigName = Util.sigName(baseModName, SOME "PICKLE")
	  val specs = List.foldr (genSpec baseModName) [] (!decls)
	  in
	    S.SIGtop(sigName, S.BASEsig specs)
	  end
      | genSig _ = raise Fail "genSig: unexpected primitive module"

  (* generate the encoder/decoder specifications for a type *)
    and genSpec modName (AST.TyDcl{id, ...}, specs) = let
	  val ty = S.CONty([], TyV.getName id)
	  val bufTy = S.CONty([], "Word8Buffer.buf")
	  val vecTy = S.CONty([], "Word8Vector.vector")
	  val sliceTy = S.CONty([], "Word8VectorSlice.slice")
	  val unitTy = S.CONty([], "unit")
	(* encoder *)
	  val encTy = S.FUNty(S.TUPLEty[bufTy, ty], unitTy)
	  val encSpc = S.VALspec(V.Type.getEncoder id, encTy)
	(* decoder *)
	  val decTy = S.FUNty(sliceTy, S.TUPLEty[ty, sliceTy])
	  val decSpc = S.VALspec(V.Type.getDecoder id, decTy)
	  in
	    encSpc :: decSpc :: specs
	  end

  (***** Structure generation *****)

    fun genStr (AST.Module{isPrim=false, id, decls}) = let
	  val baseModName = ModV.getName id
	  val sigName = Util.sigName(baseModName, SOME "PICKLE")
	  val strName = baseModName ^ "Pickle"
	  val decls = [] (* FIXME *)
	  in
	    S.STRtop(strName, SOME(false, S.IDsig sigName), S.BASEstr decls)
	  end
      | genStr _ = raise Fail "genStr: unexpected primitive module"

  end


