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

    fun gen (AST.Module{isPrim=false, id, decls}) = let
	  val baseModName = ModV.getName id
	  fun genGrp (dcls, dcls') = (case List.foldr genType ([], []) dcls
		 of ([], tbs) => List.map S.TYPEdec tbs @ dcls'
		  | (dbs, tbs) => S.DATATYPEdec(dbs, tbs) :: dcls'
		(* end case *))
	  in
	    S.STRtop(name, NONE, S.STRstr(List.foldr genGrp [] (SortDecls.sort (!decls))))
	  end
      | gen _ = raise Fail "unexpected primitive module"

  end


