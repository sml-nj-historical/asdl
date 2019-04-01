(* gen-sml.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * SML code generation for ASDL.
 *)

structure GenSML : sig

    val options : unit GetOpt.opt_descr list

    val gen : {src : string, dir : string, stem : string, modules : AST.module list} -> unit

  (* generate the optional CM and/or MLB file(s) for the list of inputs *)
    val genBuildFiles : unit -> unit

  end = struct

    structure V = SMLView
    structure ModV = V.Module
    structure TyV = V.Type
    structure ConV = V.Constr
    structure S = SML
    structure PP = TextIOPP

  (* Generate the memory pickling code for the SML view *)
    structure GenMemoryPickle = GenPickleFn (val getPklModName = ModV.getPickleName)

  (* Generate the file pickling code for the SML view *)
    structure GenFilePickle = GenPickleFn (val getPklModName = ModV.getIOName)

    val baseStructureOpt = ref "ASDL"
    val cmFileOpt = ref (NONE : string option)
    val mlbFileOpt = ref (NONE : string option)
    val genSExpFlg = ref false

    val options = [
	    { short = "", long = ["base-structure"],
	      desc = GetOpt.ReqArg(fn s => baseStructureOpt := s, "<name>"),
	      help = "specify the tructure that defines the ASDL primitive types"
	    },
	    { short = "", long = ["cm"],
	      desc = GetOpt.ReqArg(fn s => cmFileOpt := SOME s, "<name>"),
	      help = "generate a CM file"
	    },
	    { short = "", long = ["mlb"],
	      desc = GetOpt.ReqArg(fn s => mlbFileOpt := SOME s, "<name>"),
	      help = "generate an MLB file"
	    },
	    { short = "", long = ["sexp"],
	      desc = GetOpt.NoArg(fn () => genSExpFlg := true),
	      help = "generate support for S-Expression pickles"
	    }
	  ]

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
	  if Options.noOutput()
	    then print(outFile ^ "\n")
	    else output (src, outFile, List.map codeGen modules)

  (* generate the type-declaration file *)
    val genTypes = let
	  val gen = genFile GenTypes.gen
	  in
	  (* for the types, the primitive modules are supplied by the user *)
	    fn (src, outFile, modules) => gen (
		src, outFile,
		List.filter (fn (AST.Module{isPrim, ...}) => not isPrim) modules)
	  end

  (* generate the generic pickler signature *)
    val genPicklerSig = genFile GenPickleSig.gen

  (* generate the pickler files *)
    val genPicklerStr = genFile GenMemoryPickle.gen

  (* generate the pickle-io files *)
    val genIOStr = genFile GenFilePickle.gen

  (* generate the S-Expression pickler files *)
    val genSExpStr = genFile GenSExpPickle.gen

  (* references to collect together the generated files and modules so that
   * we can generate CM and/or MLB files
   *)
    val genFiles : string list ref = ref []
    val genSigs : string list ref = ref []
    val genStructs : string list ref = ref []

  (* add the generated signatures and structures to the appropriate lsits *)
    fun addModules modules = let
	  fun getSig (AST.Module{isPrim, id, ...}, sigs) = if isPrim
		then sigs
		else Util.sigName(ModV.getPickleSigName id, NONE) :: sigs
	  fun getStructs (AST.Module{id, ...}, structs) = let
		val structs = if !genSExpFlg
		      then ModV.getSExpName id :: structs
		      else structs
		in
		  ModV.getName id ::
		  ModV.getPickleName id ::
		  ModV.getIOName id ::
		    structs
		end
	  in
	    genSigs := List.foldr getSig (! genSigs) modules;
	    genStructs := List.foldr getStructs (! genStructs) modules
	  end

    fun addFile filename = (
	  genFiles := filename :: !genFiles;
	  filename)


  (* generate SML code for the given list of modules using the "Sml" view *)
    fun gen {src, dir, stem, modules} = let
	  val basePath = OS.Path.joinDirFile{dir=dir, file=stem}
	  fun smlFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "sml"}
	  fun sigFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "sig"}
	(* record the inputs before filtering out the primitive modules *)
	  val _ = addModules modules
	  in
	    genTypes (src, addFile(smlFilename basePath), modules);
	    genPicklerSig (src, addFile(sigFilename(basePath ^ "-pickle")), modules);
	    genPicklerStr (src, addFile(smlFilename(basePath ^ "-memory-pickle")), modules);
	    genIOStr (src, addFile(smlFilename(basePath ^ "-file-pickle")), modules);
	    if !genSExpFlg
	      then (
		genSExpStr (src, addFile(smlFilename(basePath ^ "-sexp-pickle")), modules))
	      else ()
	  end

  (**** CM/MLB file support ****)

    val stringSort = ListMergeSort.sort String.>

    fun genCMFile (outS, files, sigs, structs) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl ss = pr(String.concat ss)
	  fun prSig id = prl["  signature ", id, "\n"]
	  fun prStruct id = prl["  structure ", id, "\n"]
	  fun prFile file = prl["  ", file, "\n"]
	  in
	    pr "Library\n\n";
	    List.app prSig sigs;
	    pr "\n";
	    List.app prStruct structs;
	    pr "\
		\n\
		\in\n\
		\\n\
                \  $/basis.cm\n\
                \  $/asdl-lib.cm\n\
		\\n\
                \";
	    List.app prFile files
	  end

    fun genMLBFile (outS, files, sigs, structs) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prl ss = pr(String.concat ss)
	  fun prSig id = prl["  signature ", id, "\n"]
	  fun prStruct id = prl["  structure ", id, "\n"]
	  fun prFile file = prl["  ", file, "\n"]
	  in
(* FIXME: the path for the asdl-lib.mlb will depend on where MLton puts it *)
	    pr "\
		\local\n\
		\\n\
                \  $(SML_LIB)/basis/basis.mlb\n\
                \  $(ASDL_LIB)/asdl-lib.cm\n\
		\\n\
                \";
	    List.app prFile files;
	    pr "\
		\\n\
		\in\n\
		\\n\
                \";
	    List.app prSig sigs;
	    pr "\n";
	    List.app prStruct structs;
	    pr "\
		\\n\
		\end\n\
                \"
	  end

    fun genBuildFiles () = let
	  val files = stringSort (!genFiles)
	  val sigs = stringSort (!genSigs)
	  val structs = stringSort (!genStructs)
	  fun genFile gen optFile = (case !optFile
		 of SOME file => let
		      val outS = TextIO.openOut file
		      in
			gen (outS, files, sigs, structs);
			TextIO.closeOut outS
		      end
		  | NONE => ()
		(* end case *))
	  in
	    genFile genCMFile cmFileOpt;
	    genFile genMLBFile mlbFileOpt
	  end

  end
