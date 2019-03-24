(* asdl-memory-pickle.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLMemoryPickle : sig

    include ASDL_PICKLE

  (* open a vector as an input stream *)
    val openVec : Word8Vector.vector -> instream

  (* pickle to/from a vector *)
    val toVector : (outstream * 'a -> unit) -> 'a -> Word8Vector.vector
    val fromVector : (instream -> 'a)
	  -> Word8Vector.vector
	  -> 'a

  end where type outstream = Word8Buffer.buf
  = struct

    structure W = Word
    structure W8 = Word8
    structure W8V = Word8Vector
    structure W8B = Word8Buffer
    structure W8S = Word8VectorSlice

    val << = W.<< and >> = W.>>
    val ++ = W.orb
    val & = W.andb
    val !! = W.notb
    infix 5 << >>
    infix 6 ++
    infix 7 &

    type outstream = W8B.buf

    datatype instream = Instrm of {
	data : W8V.vector,
	idx : int ref,
	len : int
      }

    fun openVec v = Instrm{
	    data = v,
	    idx = ref 0,
	    len = W8V.length v
	  }

    fun toByte w = W8.fromLarge (W.toLarge w)
    fun getByte (data, ix) = W.fromLarge (W8.toLarge (W8V.sub(data, ix)))

    fun get1 (Instrm{data, idx as ref ix, len}) =
	  if (ix < len)
	    then (
	      idx := ix+1;
	      getByte(data, ix))
	    else raise ASDL.DecodeError

    fun get2 (Instrm{data, idx as ref ix, len}) = let
	  val n = ix + 2
	  in
	    if (n <= len)
	      then (
	        idx := n;
	        (getByte(data, ix), getByte(data, ix+1)))
	      else raise ASDL.DecodeError
	  end

    fun get3 (Instrm{data, idx as ref ix, len}) = let
	  val n = ix + 3
	  in
	    if (n <= len)
	      then (
	        idx := n;
	        (getByte(data, ix), getByte(data, ix+1), getByte(data, ix+2)))
	      else raise ASDL.DecodeError
	  end

    fun getSlice (Instrm{data, idx as ref ix, len}, n) =
	  if (ix + n < len)
	    then (
	      idx := ix + n;
	      W8S.slice(data, ix, SOME n))
	    else raise ASDL.DecodeError

    fun writeBool (buf, false) = W8B.add1 (buf, 0w0)
      | writeBool (buf, true) = W8B.add1 (buf, 0w1)

    fun readBool inS = (case get1 inS
	   of 0w0 => false
	    | 0w1 => true
	    | _ => raise ASDL.DecodeError
	  (* end case *))

  (* write an unsigned signed integer.  We assume that the value is in the
   * range 0..2^30 - 1
   *)
    fun writeUInt (buf, w) = if (w <= 0wx3f)
	    then W8B.add1(buf, toByte w)
	  else if (w <= 0wx3fff)
	    then ( (* two bytes *)
	      W8B.add1(buf, toByte(0wx40 ++ (w >> 0w8)));
	      W8B.add1(buf, toByte w))
	  else if (w <= 0wx3fffff)
	    then ( (* three bytes *)
	      W8B.add1(buf, toByte(0wx80 ++ (w >> 0w16)));
	      W8B.add1(buf, toByte(w >> 0w8));
	      W8B.add1(buf, toByte w))
	    else ( (* four bytes *)
	      W8B.add1(buf, toByte(0wxc0 ++ (w >> 0w24)));
	      W8B.add1(buf, toByte(w >> 0w16));
	      W8B.add1(buf, toByte(w >> 0w8));
	      W8B.add1(buf, toByte w))

    fun readUInt inS = let
	  val b0 = get1 inS
	  val nb = b0 & 0wxc0
	  val res = b0 & 0wx3f
	  in
	    if (nb = 0w0) then res
	    else if (nb = 0wx40)
	      then let
		val b1 = get1 inS
		in
		  (res << 0w8) + b1
		end
	    else if (nb = 0wx80)
	      then let
		val (b1, b2) = get2 inS
		in
		  (res << 0w16) + (b1 << 0w8) + b2
		end
	      else let
		val (b1, b2, b3) = get3 inS
		in
		  (res << 0w24) + (b1 << 0w16) + (b2 << 0w8) + b3
		end
	  end

  (* write a signed integer.  We assume that the value is in the range -2^29..2^29 - 1 *)
    fun writeInt (buf, n) = let
	  val (sign, w) = if (n < 0)
		then (0wx20, W.fromInt(~(n+1)))
		else (0w0, Word.fromInt n)
	  in
	    if (w <= 0wx1f)
	      then W8B.add1(buf, toByte(sign ++ w))
	    else if (w <= 0wx1fff)
	      then ( (* two bytes *)
		W8B.add1(buf, toByte(0wx40 ++ sign ++ (w >> 0w8)));
		W8B.add1(buf, toByte w))
	    else if (w <= 0wx1fffff)
	      then ( (* three bytes *)
		W8B.add1(buf, toByte(0wx80 ++ sign ++ (w >> 0w16)));
		W8B.add1(buf, toByte(w >> 0w8));
		W8B.add1(buf, toByte w))
	      else ( (* four bytes *)
		W8B.add1(buf, toByte(0wxc0 ++ sign ++ (w >> 0w24)));
		W8B.add1(buf, toByte(w >> 0w16));
		W8B.add1(buf, toByte(w >> 0w8));
		W8B.add1(buf, toByte w))
	  end

    fun readInt inS = let
	  val b0 = get1 inS
	  val nb = b0 & 0wxc0
	  val isNeg = (b0 & 0wx20 <> 0w0)
	  val res = b0 & 0wx1f
	  fun return w = if (b0 & 0wx20 <> 0w0)
		then (~1 - Word.toIntX w)
		else Word.toIntX w
	  in
	    if (nb = 0w0) then return res
	    else if (nb = 0wx40)
	      then let
		val b1 = get1 inS
		in
		  return ((res << 0w8) + b1)
		end
	    else if (nb = 0wx80)
	      then let
		val (b1, b2) = get2 inS
		in
		  return ((res << 0w16) + (b1 << 0w8) + b2)
		end
	      else let
		val (b1, b2, b3) = get3 inS
		in
		  return ((res << 0w24) + (b1 << 0w16) + (b2 << 0w8) + b3)
		end
	  end

    fun writeInteger (buf, n) = let
	  val (sign, n) = if n < 0 then (0wx40, ~n) else (0wx00, n)
	(* convert to sequence of 7-bit chunks in big-endian order.  Note that
	 * first chunk in result is only 6 bits to allow for the sign.
	 *)
	  fun lp (n, bs) = if (n < 64)
		then output (W.fromLargeInt n ++ sign, bs)
		else lp (IntInf.~>>(n, 0w7), (W.fromLargeInt n & 0wx7f) :: bs)
	(* output bytes to buffer with continuation bits set as necessary *)
	  and output (b, []) = W8B.add1(buf, toByte b)
	    | output (b, b'::br) = (
		W8B.add1(buf, toByte(0wx80 ++ b));
		output(b', br))
	  in
	    lp (n, [])
	  end

    fun readInteger inS = let
	(* get first byte, which include sign *)
	  val b0 = get1 inS
	(* mask out sign bit *)
	  val b0' = (b0 & 0wxbf)
	  val sign = (b0 <> b0')
	  fun return n = if sign then ~n else n
	(* get bytes in big-endian order *)
	  fun lp n = let
		val b = get1 inS
	      (* mask out continuation bit *)
		val b' = (b & 0wx7f)
		val n = n + W.toLargeInt b'
		in
		  if (b = b')
		    then return n
		    else lp (IntInf.<<(n, 0w7))
		end
	(* initial byte *)
	  val b0'' = (b0' & 0wx3f)
	  val n = W.toLargeInt b0''
	  in
	    if (b0'' = b0')
	      then return n (* only one byte *)
	      else lp (IntInf.<<(n, 0w7))
	  end

    fun writeString (buf, s) = (
	  writeUInt(buf, Word.fromInt(size s));
	  W8B.addVec(buf, Byte.stringToBytes s))

    fun readString inS = let
	  val len = W.toIntX (readUInt inS)
	  in
	    Byte.unpackStringVec(getSlice (inS, len))
	  end

    fun writeIdentifier (buf, id) = writeString (buf, Atom.toString id)

    fun readIdentifier inS = Atom.atom(readString inS)

  (* utility functions for sum-type tags *)
    fun writeTag8 (buf, tag) = W8B.add1(buf, toByte tag)
    fun readTag8 inS = get1 inS
    fun writeTag16 (buf, tag) = (
	  W8B.add1(buf, toByte(tag >> 0w8));
	  W8B.add1(buf, toByte tag));
    fun readTag16 inS = let
	  val b0 = get1 inS
	  val b1 = get1 inS
	  in
	    (b0 << 0w8) ++ b1
	  end

  (* write to a vector *)
    fun toVector write x = let
	  val buf = W8B.new 64
	  in
	    write (buf, x);
	    W8B.contents buf
	  end

    fun fromVector decode v = decode (openVec v)

  end
