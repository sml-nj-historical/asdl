(* gen-cxx.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the C++ view of the ASDL modules.
 *)

structure GenCxx : sig

    val gen : {src : string, dir : string, stem : string, modules : AST.module list} -> unit

  end = struct

    structure V = CxxView
    structure CL = Cxx

    type code = {
	hxx : CL.decl list,
	cxx : CL.decl list
      }

  (* include directives to include in the .hxx and .cxx files *)
    val hxxIncls = [
	    "#include \"asdl/asdl.hxx\"\n"
	  ]
    val cxxIncls = [
	    "#include \"@HXX_FILENAME@\"\n"
	  ]

  (* generate the file header as a verbatim top_decl *)
    fun genHeader (src, file, incls) = let
	  val hxxFile = OS.Path.joinBaseExt{
		  base = OS.Path.base file,
		  ext = SOME "hxx"
		}
	  val expand = StringSubst.expand [
		  ("FILENAME", file),
		  ("HXX_FILENAME", hxxFile),
		  ("SRCFILE", src)
		]
	  in
	    CL.D_Verbatim(List.map expand (V.File.getHeader() @ incls))
	  end

  (* output C++ declarations to a file *)
    fun output (src, outFile, incls, dcls) = let
	  val outS = TextIO.openOut outFile
(* FIXME: output width is a command-line option! *)
	  val ppStrm = TextIOPP.openOut {dst = outS, wid = Options.lineWidth()}
	  in
(* FIXME: need to insert the appropriate include files here! *)
	    List.app
	      (fn dcl => (PrintCxx.output (ppStrm, dcl)))
		(genHeader (src, outFile, incls) :: dcls);
	    TextIOPP.closeStream ppStrm;
	    TextIO.closeOut outS
	  end

  (* generate a file using the given code generator *)
    fun genFile codeGen (src, outFile, incls, modules) =
	  output (src, outFile, incls, List.map codeGen modules)

  (* generate C++ code for the given list of modules using the "Cxx" view *)
    fun gen {src, dir, stem, modules} = let
	  val basePath = OS.Path.joinDirFile{dir=dir, file=stem}
	  fun cxxFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "cxx"}
	  fun hxxFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "hxx"}
	(* we only generate code for the non-primitive modules *)
	  val modules = List.filter (fn (AST.Module{isPrim, ...}) => not isPrim) modules
	  in
	  (* generate the header file *)
	    genFile GenTypes.gen (src, hxxFilename basePath, hxxIncls, modules);
	  (* generate the pickler implementation *)
	    genFile GenPickle.gen (src, cxxFilename basePath, cxxIncls, modules)
	  end

  end
