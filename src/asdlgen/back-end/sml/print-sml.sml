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
	     of S.SIGtop(id, sigExp) => (
		  PP.openHBox strm;
		    str "signature"; sp(); str id; sp(); str "="; sp();
		  PP.closeBox strm;
		  ppSigExp (strm, sigExp);
		  nl())
	      | S.STRtop(id, optSig, strExp) => (
		  PP.openHBox strm;
		    str "structure"; sp(); str id; sp();
		    case optSig
		     of SOME(true, sigExp) => (str ":>"; sp(); ppSigExp(strm, sigExp); sp())
		      | SOME(false, sigExp) => (str ":"; sp(); ppSigExp(strm, sigExp); sp())
		      | NONE => ()
		    (* end case *);
		    str "="; sp();
		  PP.closeBox strm;
		  ppStrExp (strm, strExp);
		  nl())
	      | S.VERBtop strs => List.app str strs
	    (* end case *)
	  end

    and ppSigExp (strm, sigExp) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
	  fun ppSpec (S.STRspec _) = () (* FIXME *)
	    | ppSpec (S.TYPEspec(eqTy, tvs, tyc, rhs)) = (
		PP.openHBox strm;
		  if eqTy then str "eqtype" else str "type";
		  sp(); ppTycBind(strm, tvs, tyc);
		  case rhs
		   of SOME ty => (sp(); str "="; sp(); ppTy(strm, ty))
		    | NONE => ()
		  (* end case *);
		PP.closeBox strm)
	    | ppSpec (S.DATATYPEspec dbs) = () (* FIXME *)
	    | ppSpec (S.VALspec(id, ty)) = (
		PP.openHBox strm;
		  str "val"; sp(); str id; sp(); str ":"; sp(); ppTy(strm, ty);
		PP.closeBox strm)
	    | ppSpec (S.EXNspec con) = (
		PP.openHBox strm;
		  str "exception"; sp(); ppCon (strm, con);
		PP.closeBox strm)
	  in
	    case sigExp
	     of S.IDsig id => str id
	      | S.AUGsig(sigExp, whereTys) => () (* FIXME *)
	      | S.BASEsig specs => (
		  PP.openVBox strm indent2;
		    str "sig";
		    PP.openVBox strm indent2;
		      List.app (fn spc => (nl(); ppSpec spc)) specs;
		    PP.closeBox strm;
		    nl();
		    str "end";
		  PP.closeBox strm)
	    (* end case *)
	  end

    and ppStrExp (strm, strExp) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
	  in
	   case strExp
	    of S.IDstr id => str id
	     | S.BASEstr dcls => (
		PP.openVBox strm indent2;
		  str "struct";
		  PP.openVBox strm indent2;
		    List.app (fn dcl => (nl(); ppDec(strm, dcl))) dcls;
		  PP.closeBox strm;
		  nl();
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
		  ppTycBind (strm, tvs, tyc);
		  sp(); str "="; sp();
		  ppTy (strm, ty);
		PP.closeBox strm)
	  fun ppPat S.WILDpat = str "_"
	    | ppPat (S.IDpat id) = str id
	    | ppPat (S.NUMpat n) = str n
	    | ppPat (S.STRINGpat s) = str(concat["\"", String.toString s, "\""])
	    | ppPat (S.CHARpat s) = str(concat["#\"", String.toString s, "\""])
	    | ppPat (S.CONpat(c, p)) = ??
	    | ppPat (S.RECORDpat{fields, flex}) = ??
	    | ppPat (S.TUPLEpat ps) = ??
	    | ppPat (S.CONSTRAINTpat(p, ty)) = ??
	    | ppPat (S.ASpat(x, p)) = ??
	  fun ppExp (S.IDexp id) = str id
	    | ppExp (S.NUMexp n) = str n
	    | ppExp (S.STRINGexp s) = str(concat["\"", String.toString s, "\""])
	    | ppExp (S.CHARexp s) = str(concat["#\"", String.toString s, "\""])
(*
	    | ppExp (S.RECORDexp of (id * exp) list
	    | ppExp (S.TUPLEexp of exp list
	    | ppExp (S.SELECTexp of id * exp
	    | ppExp (S.APPexp of exp * exp
	    | ppExp (S.HANDLEexp of exp * (pat * exp) list
	    | ppExp (S.RAISEexp of exp
	    | ppExp (S.CASEexp of exp * (pat * exp) list
	    | ppExp (S.IFexp(e1, e2, e3)) =
	    | ppExp (S.ANDALSOexp of exp * exp
	    | ppExp (S.ORELSEexp of exp * exp
	    | ppExp (S.FNexp of (pat * exp) list
	    | ppExp (S.LETexp of dec list * exp
	    | ppExp (S.SEQexp es) =
	    | ppExp (S.CONSTRAINTexp of exp * ty
*)
	    | ppExp (S.VERBexp s) = str s
	  in
	    case dcl
	     of S.VALdec(pat, exp) => (
		  PP.openHBox ppStrm;
		    str "val"; sp(); ppPat pat; sp(); str "="; sp(); ppExp exp;
		  PP.closeBox)
	      | S.FUNdec fbs => () (* FIXME *)
	      | S.TYPEdec tb => ppTB ("type", tb)
	      | S.DATATYPEdec(dbs, tbs) => let
		  fun db (S.DB(tvs, tyc, cons), isFirst) = (
			PP.openHBox strm;
			  if isFirst
			    then str "datatype"
			    else (nl(); str "and");
			  sp(); ppTycBind (strm, tvs, tyc);
			  case cons
			   of [] => raise Fail "impossible"
			    | [con] => (sp(); str "="; sp(); ppCon(strm, con))
			    | cons => let
				fun ppCon' (con, isFirst) = (
				      nl();
				      PP.openHBox strm;
					if isFirst then str "=" else str "|";
					sp(); ppCon(strm, con);
				      PP.closeBox strm;
				      false)
				in
				  PP.openVBox strm indent2;
				    List.foldl ppCon' true cons;
				  PP.closeBox strm
				end
			  (* end case *);
			PP.closeBox strm;
			false)
		  fun tb (tyb, prefix) = (nl(); ppTB(prefix, tyb); "and")
		  in
		    PP.openVBox strm indent0;
		      List.foldl db true dbs;
		      List.foldl tb "withtype" tbs;
		    PP.closeBox strm
		  end
	      | S.EXCEPTIONdec con => (
		  PP.openHBox strm;
		    str "exception"; sp(); ppCon (strm, con);
		  PP.closeBox strm)
	      | S.STRdec(id, sign, strExp) => () (* FIXME *)
	      | S.OPENdec ids => () (* FIXME *)
	      | S.LOCALdec(dcls1, dcls2) => () (* FIXME *)
	      | S.VERBdec strs => List.app str strs
	    (* end case *)
	  end

    and ppTycBind (strm, tvs, tyc) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
	  in
	    case tvs
	     of [] => ()
	      | [tv] => (str tv; sp())
	      | tvs => (str "("; str(String.concatWith "," tvs); str ")"; sp())
	    (* end case *);
	    str tyc
	  end

    and ppTy (strm, ty) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
	  fun pp (S.VARty tv) = str tv
	    | pp (S.CONty([], tyc)) = str tyc
	    | pp (S.CONty([ty], tyc)) = (atomic ty; sp(); str tyc)
	    | pp (S.CONty(ty::tys, tyc)) = (
		str "(";
		pp ty;
		List.app (fn ty => (str ","; sp(); pp ty)) tys;
		str ")"; sp(); str tyc)
	    | pp (S.FUNty(ty1 as S.FUNty _, ty2)) = (atomic ty1; sp(); str "->"; sp(); pp ty2)
	    | pp (S.FUNty(ty1, ty2)) = (pp ty1; sp(); str "->"; sp(); pp ty2)
	    | pp (S.RECORDty fields) = let
		fun field (label, ty) = (str label; sp(); str ":"; sp(); pp ty)
		in
		  str "{";
		  case fields
		   of [] => ()
		    | [fld] => field fld
		    | fld::flds => (
			field fld;
			List.app (fn fld => (str ","; sp(); field fld)) flds)
		  (* end case *);
		  str "}"
		end
	    | pp (S.TUPLEty[]) = str "unit"
	    | pp (S.TUPLEty[ty]) = pp ty
	    | pp (S.TUPLEty(ty::tys)) = (
		atomic ty;
		List.app (fn ty => (sp(); str "*"; sp(); atomic ty)) tys)
	    | pp (S.VERBty ty) = str ty
	  and atomic ty = let
		fun paren () = (str "("; pp ty; str ")")
		in
		  case ty
		   of (S.TUPLEty _) => paren()
		    | (S.FUNty _) => paren()
		    | _ => pp ty
		  (* end case *)
		end
	  in
	    PP.openHBox strm;
	      pp ty;
	    PP.closeBox strm
	  end

    and ppCon (strm, con) = let
          val str = PP.string strm
	  fun sp () = PP.space strm 1
	  in
	    case con
	     of (id, NONE) => str id
	      | (id, SOME ty) => (
		  PP.openHBox strm;
		    str id; sp(); str "of"; sp(); ppTy(strm, ty);
		  PP.closeBox strm)
	    (* end case *)
	  end

    fun output strm decl = (
	  PP.openVBox strm indent0;
	    ppTopDecl (strm, decl);
	    PP.newline strm;
	  PP.closeBox strm)

  end
