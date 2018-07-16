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

  (* generate SML code for the given list of modules using the "Sml" view *)
    fun genFile {dir, stem, modules} = let
	  in
	  end

    and genModule (AST.Module{isPrim=false, id, decls}) = let
	  val name = V.getName id
	  in
	  end

  end