(* asdl-pickle-io.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLPickleIO : ASDL_PICKLE_IO =
  struct

    structure W8 = Word8
    structure W8B = Word8Buffer
    structure W8S = Word8VectorSlice

    val << = W.<< and >> = W.>>
    val ++ = W.orb
    val & = W.andb
    val !! = W.notb
    infix 5 << >>
    infix 6 ++
    infix 7 &

    fun toByte w = W8.fromLarge (W.toLarge w)
    fun fromByte b = W.fromLarge (W8.toLarge b)

  (* read a single byte as a word *)
    fun readByte inS = (case BinOP.input1 inS
	   of SOME b => fromByte b
	    | NONE => raise ASDL.DecodeError
	  (* end case *))

    fun writeBool (outS, false) = BinIO.output1(outS, 0w0)
      | writeBool (outS, true) = BinIO.output1(outS, 0w1)

    fun readBool inS = (case BinIO.input1 inS
	   of SOME 0w0 => false
	    | SOME 0w1 => true
	    | _ => raise ASDL.DecodeError
	  (* end case *))

(* TODO
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
*)

  (* utility functions for sum-type tags *)
    fun encodeTag8 (outS, tag) = BinIO.output1(outS, toByte tag)
    val decodeTag8 = readByte
    val encodeTag16 (outS, tag) = (
	  BinIO.output1(outS, toByte(tag >> 0w8));
	  BinIO.output1(outS, toByte tag));
    val decodeTag16 inS = ((getByte inS) << 0w8) ++ (getByte inS)

  end
