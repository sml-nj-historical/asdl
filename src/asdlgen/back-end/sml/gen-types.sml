(* gen-types.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the SML type definitions for an ASDL module.
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
    structure ConV = V.Constr
    structure S = SML

    fun gen (AST.Module{isPrim=false, id, decls}) = let
	  val name = ModV.getName id
	  fun genGrp (dcls, dcls') = (case List.foldr genType ([], []) dcls
		 of ([], tbs) => List.map S.TYPEdec tbs @ dcls'
		  | (dbs, tbs) => S.DATATYPEdec(dbs, tbs) :: dcls'
		(* end case *))
	  in
	    S.STRtop(name, NONE, S.BASEstr(List.foldr genGrp [] (SortDecls.sort (!decls))))
	  end
      | gen _ = raise Fail "unexpected primitive module"

    and genType (AST.TyDcl{id, def, ...}, (dbs, tbs)) = let
	  val name = TyV.getName id
	  fun db cons = let
		fun con (AST.Constr{id, fields=[], ...}) = (ConV.getName id, NONE)
		  | con (AST.Constr{id, fields, ...}) = (ConV.getName id, SOME(genProdTy fields))
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
      | genProdTy [{label=AST.Pos _, ty}] = genTyExp ty
      | genProdTy (fields as {label=AST.Pos _, ...}::_) =
	  S.TUPLEty(List.map (genTyExp o #ty) fields)
      | genProdTy fields = let
	  fun field {label=AST.Lab lab, ty} = (lab, genTyExp ty)
	    | field _ = raise Fail "missing label in record type"
	  in
	    S.RECORDty(List.map field fields)
	  end

    and genTyExp (AST.Typ(ty, tyc)) = let
	  val tyName = (case ty
		 of AST.BaseTy tyId => TyV.getName tyId
		  | AST.ImportTy(modId, tyId) => String.concat[
			ModV.getName modId, ".", TyV.getName tyId
		      ]
		  | AST.LocalTy(AST.TyDcl{id, ...}) => TyV.getName id
		(* end case *))
	  val ty' = S.CONty([], tyName)
	  in
	    case tyc
	     of AST.NoTyc => ty'
	      | AST.OptTyc => S.CONty([ty'], "option")
	      | AST.SeqTyc => S.CONty([ty'], "list")
	      | AST.SharedTyc => raise Fail "FIXME: shared types not implemented"
	    (* end case *)
	  end

  end


