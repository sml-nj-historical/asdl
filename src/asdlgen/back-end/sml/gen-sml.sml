(* gen-sml.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * SML code generation for ASDL.
 *)

structure GenSML : sig

    val gen : {src : string, dir : string, stem : string, modules : AST.module list} -> unit

  end = struct

    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure S = SML
    structure PP = TextIOPP

  (* generate the file header as a verbatim top_decl *)
    fun genHeader (src, file) = let
	  val expand = StringSubst.expand [
		  ("FILENAME", file),
		  ("SRCFILE", src)
		]
	  in
	    S.VERBtop(List.map expand (V.File.getHeader()))
	  end

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
    fun genTypes (src, outFile, modules) =
	  output (outFile, genHeader (src, outFile) :: List.map GenTypes.gen modules)

  (* generate SML code for the given list of modules using the "Sml" view *)
    fun gen {src, dir, stem, modules} = let
	  val basePath = OS.Path.joinDirFile{dir=dir, file=stem}
	  in
	    genTypes (src, OS.Path.joinBaseExt{base=basePath, ext=SOME "sml"}, modules)
	  end

  end
