(* main.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Main : sig

    val main : string * string list -> OS.Process.status

  end = struct

    fun main (cmdName, args) = OS.Process.Failure (* FIXME *)

  end
