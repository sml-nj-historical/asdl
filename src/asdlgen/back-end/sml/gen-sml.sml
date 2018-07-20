(* gen-sml.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * SML code generation for ASDL.
 *)

structure GenSML : sig

    val gen : {dir : string, stem : string, modules : AST.module list} -> unit

  end = struct

    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure S = SML
    structure PP = TextIOPP

  (* output SML declarations to a file *)
    fun output (outFile, dcls) = let
	  val outS = TextIO.openOut outFile
(* FIXME: output width is a command-line option! *)
	  val ppStrm = TextIOPP.openOut {dst = outS, wid = Options.lineWidth()}
	  in
	    List.map (PrintSML.output ppStrm) dcls;
	    TextIOPP.closeStream ppStrm;
	    TextIO.closeOut outS
	  end

  (* generate the type declarations *)
    fun genTypes (outFile, modules) =
	  output (outFile, List.map GenTypes.gen modules)

  (* generate SML code for the given list of modules using the "Sml" view *)
    fun gen {dir, stem, modules} = let
	  val basePath = OS.Path.joinDirFile{dir=dir, file=stem}
	  in
	    genTypes (OS.Path.joinBaseExt{base=basePath, ext=SOME "sml"}, modules)
	  end

  end
