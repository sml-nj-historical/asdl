(* asdl-pickle.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ASDL_PICKLE =
  sig

    val write_bool : BinIO.outstream * ASDL.bool -> unit
    val read_bool : BinIO.instream -> ASDL.bool

    val write_int : BinIO.outstream * ASDL.int -> unit
    val read_int : BinIO.instream -> ASDL.int

    val write_uint : BinIO.outstream * ASDL.uint -> unit
    val read_uint : BinIO.instream -> ASDL.uint

    val write_integer : BinIO.outstream * ASDL.integer -> unit
    val read_integer : BinIO.instream -> ASDL.integer

    val write_string : BinIO.outstream * ASDL.string -> unit
    val read_string : BinIO.instream -> ASDL.string

    val write_identifier : BinIO.outstream * ASDL.identifier -> unit
    val read_identifier : BinIO.instream -> ASDL.identifier

  end



