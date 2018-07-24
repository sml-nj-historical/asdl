(* asdl-pickle.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ASDL_PICKLE =
  sig

    val encode_bool : Word8Buffer.buffer * ASDL.bool -> unit
    val decode_bool : Word8VectorSlice.slice -> ASDL.bool * Word8VectorSlice.slice

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

  end



