(* asdl-pickle.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ASDL_PICKLE =
  sig

    val encodeBool : Word8Buffer.buf * ASDL.bool -> unit
    val decodeBool : Word8VectorSlice.slice -> ASDL.bool * Word8VectorSlice.slice

    val encodeInt : Word8Buffer.buf * ASDL.int -> unit
    val decodeInt : Word8VectorSlice.slice -> ASDL.int * Word8VectorSlice.slice

    val encodeUInt : Word8Buffer.buf * ASDL.uint -> unit
    val decodeUInt : Word8VectorSlice.slice -> ASDL.uint * Word8VectorSlice.slice

    val encodeInteger : Word8Buffer.buf * ASDL.integer -> unit
    val decodeInteger : Word8VectorSlice.slice -> ASDL.integer * Word8VectorSlice.slice

    val encodeString : Word8Buffer.buf * ASDL.string -> unit
    val decodeString : Word8VectorSlice.slice -> ASDL.string * Word8VectorSlice.slice

    val encodeIdentifier : Word8Buffer.buf * ASDL.identifier -> unit
    val decodeIdentifier : Word8VectorSlice.slice -> ASDL.identifier * Word8VectorSlice.slice

  (* utility functions for sum-type tags *)
    val encodeTag8 : Word8Buffer.buf * word -> unit
    val decodeTag8 : Word8VectorSlice.slice -> word * Word8VectorSlice.slice
    val encodeTag16 : Word8Buffer.buf * word -> unit
    val decodeTag16 : Word8VectorSlice.slice -> word * Word8VectorSlice.slice

  end
