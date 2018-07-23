(* main.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The driver for asdl-gen.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

  (* register the supported views *)
    val () = List.app Options.registerGen [
(*
	    (["c++", "cxx"],	GENERATE ??),
*)
	    (["sml"],		GenSML.gen)
(*
	    (["typ"],		GENERATE ??),
*)
	  ]

    fun doFile gen file = let
	  fun getStem file = (case OS.Path.splitBaseExt file
		 of {base, ext=SOME "asdl"} => base
		  | _ => file
		(* end case *))
	  in
	    case FrontEnd.doFile file
	     of SOME{modules} => let
		  val (dir, stem) = (case (Options.outputDir(), OS.Path.splitDirFile file)
			 of (NONE, {dir, file}) => (dir, getStem file)
			  | (SOME dir, {file, ...}) => (dir, getStem file)
			(* end case *))
		  in
		    gen {src = file, dir = dir, stem = stem, modules = modules};
		    false
		  end
	      | NONE => true
	    (* end case *)
	  end

    fun fail (cmdName, msg) = (
	  TextIO.output(TextIO.stdErr, concat[
	      cmdName, ": ", msg, "\n", Options.usage()
	    ]);
	  OS.Process.failure)

    fun main (cmdName, args) = let
	  val {command, files} = Options.parseCmdLine args
	  in
	    case command
	     of Options.HELP => (
		  TextIO.output(TextIO.stdOut, Options.usage());
		  OS.Process.success)
	      | Options.VERSION => (
		  TextIO.output(TextIO.stdOut, Config.version ^ "\n");
		  OS.Process.success)
	      | Options.CHECK => OS.Process.success (* FIXME *)
	      | Options.GENERATE gen => if List.exists (doFile gen) files
		  then OS.Process.failure
		  else OS.Process.success
	    (* end case *)
	  end
	    handle Options.Usage msg => fail (cmdName, msg)
		| Fail msg => fail (cmdName, "uncaught exception Fail: " ^ msg)
		| ex => fail (cmdName, "uncaught exception " ^ exnName ex)

  end
