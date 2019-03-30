(* asdl-pickle.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common interface for basic pickling operations.
 *)

signature ASDL_PICKLE =
  sig

    type instream
    type outstream

    val writeBool : outstream * ASDL.bool -> unit
    val readBool : instream -> ASDL.bool

    val writeInt : outstream * ASDL.int -> unit
    val readInt : instream -> ASDL.int

    val writeUInt : outstream * ASDL.uint -> unit
    val readUInt : instream -> ASDL.uint

    val writeInteger : outstream * ASDL.integer -> unit
    val readInteger : instream -> ASDL.integer

    val writeString : outstream * ASDL.string -> unit
    val readString : instream -> ASDL.string

    val writeIdentifier : outstream * ASDL.identifier -> unit
    val readIdentifier : instream -> ASDL.identifier

  (* utility functions for sum-type tags *)
    val writeTag8 : outstream * word -> unit
    val readTag8 : instream -> word
    val writeTag16 : outstream * word -> unit
    val readTag16 : instream -> word

  end
