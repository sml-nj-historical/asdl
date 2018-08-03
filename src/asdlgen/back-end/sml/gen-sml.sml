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
    fun output (src, outFile, dcls) = let
	  val outS = TextIO.openOut outFile
(* FIXME: output width is a command-line option! *)
	  val ppStrm = TextIOPP.openOut {dst = outS, wid = Options.lineWidth()}
	  in
	    List.app (PrintSML.output ppStrm) (genHeader (src, outFile) :: dcls);
	    TextIOPP.closeStream ppStrm;
	    TextIO.closeOut outS
	  end

  (* generate a file using the given code generator *)
    fun genFile codeGen (src, outFile, modules) =
	  output (src, outFile, List.map codeGen modules)

  (* generate the type-declaration file *)
    val genTypes = genFile GenTypes.gen

  (* generate the pickler files *)
    val genPicklerSig = genFile GenPickle.genSig
    val genPicklerStr = genFile GenPickle.genStr

  (* generate the pickle-io files *)
    val genIOSig = genFile GenIO.genSig
    val genIOStr = genFile GenIO.genStr

  (* generate SML code for the given list of modules using the "Sml" view *)
    fun gen {src, dir, stem, modules} = let
	  val basePath = OS.Path.joinDirFile{dir=dir, file=stem}
	  fun smlFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "sml"}
	  fun sigFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "sig"}
	  in
	    genTypes (src, smlFilename basePath, modules);
	    genPicklerSig (src, sigFilename(basePath ^ "-pickle"), modules);
	    genPicklerStr (src, smlFilename(basePath ^ "-pickle"), modules);
	    genIOSig (src, sigFilename(basePath ^ "-pickle-io"), modules);
	    genIOStr (src, smlFilename(basePath ^ "-pickle-io"), modules)
	  end

  end
