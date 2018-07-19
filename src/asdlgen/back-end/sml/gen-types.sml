(* gen-types.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure GenTypes : sig

  (* generate the structure that contains the type definitions for an
   * ASDL module.
   *)
    val gen : AST.module -> SML.top_decl

  end = struct

    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Con
    structure S = SML

    fun gen (AST.Module{isPrim=false, id, decls}, dcls) = let
	  val name = ModV.getName id
	  fun genGrp (dcls, dcls') = (case List.foldr genTyp ([], []) dcls
		 of ([], tbs) => List.map S.TYPEdec tbs @ dcls'
		  | (dbs, tbs) => S.DATATYPEdec(dbs, tbs) :: dcls
		(* end case *))
	  in
	    S.STRtop(name, NONE, List.foldr genGrp [] (SortDecls.sort decls))
	  end

    and genType (AST.TyDcl{id, def, ...}, (dbs, tbs)) = let
	  val name = TyV.getName id
	  fun db cons = let
		fun con (AST.Constr{id, fields, ...}) = (ConV.getName id, genProdTy fields)
		in
		  (S.DB([], name, List.map con cons)::dbs, tbs)
		end
	  in
	    case !def
	     of AST.EnumTy cons => db cons
	      | AST.SumTy{attribs, cons} => db cons
	      | AST.ProdTy{fields} => (dbs, ([], name, genProdTy fields)::tbs)
	      | AST.PrimTy => raise Fail "unexpected primitive type"
	    (* end case *)
	  end

  (* generate a type expression for a list of fields *)
    and genProdTy [] = S.CONty([], "unit")
      | genProdTy [{label=NONE, ty}] = genTyExp ty
      | genProdTy (fields as {label=NONE, ...}::_) =
	  S.TUPLEty(List.map (genTyExp o #ty) fields)
      | genProdTy fields = let
	  fun field {label=SOME lab, ty} = (lab, genTyExp ty)
	    | field _ = raise Fail "missing label in record type"
	  in
	    S.RECORDty(List.map field fields)
	  end

    and genTyExp (AST.Typ(ty, tyc)) = let
	  val ty' = (case ty
		 of AST.BaseTy tyId => TyV.name tyId
		  | AST.ImportTy(modId, tyId) => String.concat[ModV.name modId, ".", TyV.name tyId]
		  | AST.LocalTy(AST.TyDcl{id, ...}) => TyV.name id
		(* end case *))
	  in
	    case tyc
	     of AST.NoTyc => ty'
	      | AST.OptTyc => S.CONty([ty'], "option")
	      | AST.SeqTyc => S.CONty([ty'], "list")
	      | AST.SharedTyc => raise Fail "FIXME: shared types not implemented"
	    (* end case *)
	  end

  end


