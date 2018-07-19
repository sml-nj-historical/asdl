(* gen-sml.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * SML code generation for ASDL.
 *)

structure GenSML : sig

  end = struct

    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Con

  (* generate SML code for the given list of modules using the "Sml" view *)
    fun genFile {dir, stem, modules} = let
	  in
	  end

    and genModule (AST.Module{isPrim=false, id, decls}) = let
	  val name = ModV.getName id
	  in
	  end

    and genType (AST.TyDcl{id, def, ...}) = let
	  val name = TyV.getName id
	  in
	    case !def
	     of AST.EnumTy cons =>
	      | AST.SumTy{attribs, cons} =>
	      | AST.ProdTy{fields} =>
	      | AST.PrimTy => raise Fail "unexpected primitive type"
	    (* end case *)
	  end

  end
