(* asdl-pickle.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLPickle : ASDL_PICKLE =
  struct

    structure W8 = Word8
    structure W8B = Word8Buffer
    structure W8S = Word8VectorSlice

    fun encode_bool (buf, false) = W8B.add1(buf, 0w0)
      | encode_bool (buf, true) = W8B.add1(buf, 0w1)

    fun decode_bool slice = (case W8S.getItem slice
	   of SOME(0w0, slice) => (false, slice)
	    | SOME(0w1, slice) => (true, slice)
	    | _ => raise ASDL.DecodeError
	  (* end case *))

(* TODO
    val encode_int : Word8Buffer.buffer * ASDL.int -> unit
    val decode_int : Word8VectorSlice.slice -> ASDL.int * Word8VectorSlice.slice

    val encode_uint : Word8Buffer.buffer * ASDL.uint -> unit
    val decode_uint : Word8VectorSlice.slice -> ASDL.uint * Word8VectorSlice.slice

    val encode_integer : Word8Buffer.buffer * ASDL.integer -> unit
    val decode_integer : Word8VectorSlice.slice -> ASDL.integer * Word8VectorSlice.slice

    val encode_string : Word8Buffer.buffer * ASDL.string -> unit
    val decode_string : Word8VectorSlice.slice -> ASDL.string * Word8VectorSlice.slice

    val encode_identifier : Word8Buffer.buffer * ASDL.identifier -> unit
    val decode_identifier : Word8VectorSlice.slice -> ASDL.identifier * Word8VectorSlice.slice
*)

  end
