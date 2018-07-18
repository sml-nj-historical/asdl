(* main.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    fun main (cmdName, args) = let
	  val {files, ...} = Options.parseCmdLine args
	  in
(* FIXME *)
	    OS.Process.Success
	  end
	    handle Options.Usage msg => (
	      TextIO.output(TextIO.stdErr, concat[
		  cmdName, ": ", msg, "\n", Options.usage()
		]);
	      OS.Process.Failure)

  end
