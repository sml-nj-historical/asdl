(* asdl-types.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ASDL_TYPES =
  sig

    type nat = Word.word
    type bool = Bool.bool
    type ieee_real = Real.real

    type int8 = Int.int
    type int16 = Int.int
    type int32 = Int32.int
    type int64 = Word64.int

    type uint8 = Word8.word
    type uint16 = Word.word
    type uint32 = Word32.word
    type uint64 = Word64.word

  end
