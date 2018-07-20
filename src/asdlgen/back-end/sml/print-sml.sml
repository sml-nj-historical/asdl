(* print-sml.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PrintSML : sig

    val output : TextIOPP.stream -> SML.top_decl -> unit

  end = struct

    structure PP = TextIOPP
    structure S = SML

    val indent0 = (PP.Abs 0)
    val indent2 = (PP.Abs 2)
    val indent4 = (PP.Abs 4)

    fun ppTopDecl (strm, dcl) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
	  in
	    case dcl
	     of S.SIGtop(id, sigExp) => () (* FIXME *)
	      | S.STRtop(id, NONE, strExp) => (
		  PP.openHBox strm;
		    str "structure"; sp(); str id; sp(); str "="; sp();
		  PP.closeBox strm;
		  ppStrExp (strm, strExp);
		  nl())
	      | S.STRtop(id, SOME(isOpaque, sigExp), strExp) => () (* FIXME *)
	      | S.VERBtop strs => List.app str strs
	    (* end case *)
	  end

    and ppStrExp (strm, strExp) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
	  in
	   case strExp
	    of S.IDstr id => str id
	     | S.STRstr dcls => (
		PP.openVBox strm indent2;
		  str "struct";
		  PP.openVBox strm indent2;
		    List.app (fn dcl => (nl(); ppDec(strm, dcl))) dcls;
		  PP.closeBox strm;
		  str "end";
		PP.closeBox strm)
	     | S.VERBstr strs => List.app str strs
	    (* end case *)
	  end

    and ppDec (strm, dcl) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
	  fun ppTB (prefix, (tvs, tyc, ty)) = (
		PP.openHBox strm;
		  str prefix; sp();
		  case tvs
		   of [] => ()
		    | [tv] => (str tv; sp())
		    | tvs => (str "("; str(String.concatWith "," tvs); str ")"; sp())
		  (* end case *);
		  str tyc; sp(); str "="; sp();
		  ppTy (strm, ty);
		PP.closeBox strm)
	  fun ppCon (id, NONE) = str id
	    | ppCon (id, SOME ty) = (
		PP.openHBox strm;
		  str id; sp(); str "of"; sp(); ppTy(strm, ty);
		PP.closeBox strm)
	  in
	    case dcl
	     of S.VALdec(pat, exp) => () (* FIXME *)
	      | S.FUNdec fbs => () (* FIXME *)
	      | S.TYPEdec tb => ppTB ("type", tb)
	      | S.DATATYPEdec(dbs, tbs) => () (* FIXME *)
	      | S.EXCEPTIONdec(id, optTy) => (
		  PP.openHBox strm;
		    str "exception"; sp(); ppCon (id, optTy);
		  PP.closeBox strm)
	      | S.STRdec(id, sign, strExp) => () (* FIXME *)
	      | S.OPENdec ids => () (* FIXME *)
	      | S.LOCALdec(dcls1, dcls2) => () (* FIXME *)
	      | S.VERBdec strs => List.app str strs
	    (* end case *)
	  end

    and ppTy (strm, ty) = () (* FIXME *)

    fun output strm decl = (
	  PP.openVBox strm indent0;
	    ppTopDecl (strm, decl);
	    PP.newline strm;
	  PP.closeBox strm)

  end
