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
    structure S = SML

  (* generate SML code for the given list of modules using the "Sml" view *)
    fun genFile {dir, stem, modules} = let
	  in
	  end

  end
