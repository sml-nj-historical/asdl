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

    fun write_bool (outS, false) = BinIO.output1(outS, 0w0)
      | write_bool (outS, true) = BinIO.output1(outS, 0w1)

    fun read_bool inS = (case BinIO.input1 inS
	   of SOME 0w0 => false
	    | SOME 0w1 => true
	    | _ => raise ASDL.DecodeError
	  (* end case *))

(* TODO
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
*)

  end
