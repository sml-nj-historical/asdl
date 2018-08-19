(* asdl-pickle-io.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ASDL_PICKLE_IO =
  sig

    val writeBool : BinIO.outstream * ASDL.bool -> unit
    val readBool : BinIO.instream -> ASDL.bool

    val writeInt : BinIO.outstream * ASDL.int -> unit
    val readInt : BinIO.instream -> ASDL.int

    val writeUInt : BinIO.outstream * ASDL.uint -> unit
    val readUInt : BinIO.instream -> ASDL.uint

    val writeInteger : BinIO.outstream * ASDL.integer -> unit
    val readInteger : BinIO.instream -> ASDL.integer

    val writeString : BinIO.outstream * ASDL.string -> unit
    val readString : BinIO.instream -> ASDL.string

    val writeIdentifier : BinIO.outstream * ASDL.identifier -> unit
    val readIdentifier : BinIO.instream -> ASDL.identifier

  (* utility functions for sum-type tags *)
    val writeTag8 : BinIO.outstream * word -> unit
    val readTag8 : BinIO.instream -> word
    val writeTag16 : BinIO.outstream * word -> unit
    val readTag16 : BinIO.instream -> word

  end
