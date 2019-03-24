(* asdl-file-pickle.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLFilePickle : sig

    include ASDL_PICKLE

  (* pickle to/from files *)
    val toFile : (outstream * 'a -> unit) -> (string * 'a) -> unit
    val fromFile : (instream -> 'a) -> string -> 'a

  end where type instream = BinIO.instream
      where type outstream = BinIO.outstream
  = struct

    structure W = Word
    structure W8 = Word8
    structure W8V = Word8Vector

    val << = W.<< and >> = W.>>
    val ++ = W.orb
    val & = W.andb
    val !! = W.notb
    infix 5 << >>
    infix 6 ++
    infix 7 &

    type instream = BinIO.instream
    type outstream = BinIO.outstream

    fun toByte w = W8.fromLarge (W.toLarge w)
    fun fromByte b = W.fromLarge (W8.toLarge b)

  (* read a single byte as a word *)
    fun readByte inS = (case BinIO.input1 inS
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

  (* write an unsigned signed integer.  We assume that the value is in the
   * range 0..2^30 - 1
   *)
    fun writeUInt (outS, w) = if (w <= 0wx3f)
	    then BinIO.output1(outS, toByte w)
	  else if (w <= 0wx3fff)
	    then ( (* two bytes *)
	      BinIO.output1(outS, toByte(0wx40 ++ (w >> 0w8)));
	      BinIO.output1(outS, toByte w))
	  else if (w <= 0wx3fffff)
	    then ( (* three bytes *)
	      BinIO.output1(outS, toByte(0wx80 ++ (w >> 0w16)));
	      BinIO.output1(outS, toByte(w >> 0w8));
	      BinIO.output1(outS, toByte w))
	    else ( (* four bytes *)
	      BinIO.output1(outS, toByte(0wxc0 ++ (w >> 0w24)));
	      BinIO.output1(outS, toByte(w >> 0w16));
	      BinIO.output1(outS, toByte(w >> 0w8));
	      BinIO.output1(outS, toByte w))

    fun readUInt inS = let
	  val b0 = readByte inS
	  val nb = b0 & 0wxc0
	  val res = b0 & 0wx3f
	  in
	    if (nb = 0w0) then res
	    else let
	      val b1 = readByte inS
	      val res = (res << 0w8) + b1
	      in
		if (nb = 0wx40) then res
		else let
		  val b2 = readByte inS
		  val res = (res << 0w8) + b2
		  in
		    if (nb = 0wx80) then res
		    else let
		      val b3 = readByte inS
		      in
			(res << 0w8) + b3
		      end
		  end
	      end
	  end


  (* encode a signed integer.  We assume that the value is in the range -2^29..2^29 - 1 *)
    fun writeInt (outS, n) = let
	  val (sign, w) = if (n < 0)
		then (0wx20, W.fromInt(~(n+1)))
		else (0w0, Word.fromInt n)
	  in
	    if (w <= 0wx1f)
	      then BinIO.output1(outS, toByte(sign ++ w))
	    else if (w <= 0wx1fff)
	      then ( (* two bytes *)
		BinIO.output1(outS, toByte(0wx40 ++ sign ++ (w >> 0w8)));
		BinIO.output1(outS, toByte w))
	    else if (w <= 0wx1fffff)
	      then ( (* three bytes *)
		BinIO.output1(outS, toByte(0wx80 ++ sign ++ (w >> 0w16)));
		BinIO.output1(outS, toByte(w >> 0w8));
		BinIO.output1(outS, toByte w))
	      else ( (* four bytes *)
		BinIO.output1(outS, toByte(0wxc0 ++ sign ++ (w >> 0w24)));
		BinIO.output1(outS, toByte(w >> 0w16));
		BinIO.output1(outS, toByte(w >> 0w8));
		BinIO.output1(outS, toByte w))
	  end

    fun readInt inS = let
	  val b0 = readByte inS
	  val nb = b0 & 0wxc0
	  val isNeg = (b0 & 0wx20 <> 0w0)
	  val res = b0 & 0wx1f
	  fun return w = if (b0 & 0wx20 <> 0w0)
		then ~1 - Word.toIntX w
		else Word.toIntX w
	  in
	    if (nb = 0w0) then return res
	    else let
	      val b1 = readByte inS
	      val res = (res << 0w8) + b1
	      in
		if (nb = 0wx40) then return res
		else let
		  val b2 = readByte inS
		  val res = (res << 0w8) + b2
		  in
		    if (nb = 0wx80) then return res
		    else let
		      val b3 = readByte inS
		      in
			return((res << 0w8) + b3)
		      end
		  end
	      end
	  end

    fun writeInteger (outS, n) = let
	  val (sign, n) = if n < 0 then (0wx40, ~n) else (0wx00, n)
	(* convert to sequence of 7-bit chunks in big-endian order.  Note that
	 * first chunk in result is only 6 bits to allow for the sign.
	 *)
	  fun lp (n, bs) = if (n < 64)
		then output (W.fromLargeInt n ++ sign, bs)
		else lp (IntInf.~>>(n, 0w7), (W.fromLargeInt n & 0wx7f) :: bs)
	(* output bytes to buffer with continuation bits set as necessary *)
	  and output (b, []) = BinIO.output1(outS, toByte b)
	    | output (b, b'::br) = (
		BinIO.output1(outS, toByte(0wx80 ++ b));
		output(b', br))
	  in
	    lp (n, [])
	  end

    fun readInteger inS = let
	(* get first byte, which include sign *)
	  val b0 = readByte inS
	(* mask out sign bit *)
	  val b0' = (b0 & 0wxbf)
	  val sign = (b0 <> b0')
	  fun return n = if sign then ~n else n
	(* get bytes in big-endian order *)
	  fun lp n = let
		val b = readByte inS
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

    fun writeString (outS, s) = (
	  writeUInt(outS, Word.fromInt(size s));
	  BinIO.output(outS, Byte.stringToBytes s))

    fun readString inS = let
	  val len = W.toIntX (readUInt inS)
	  val bytes = BinIO.inputN (inS, len)
	  in
	    if W8V.length bytes <> len
	      then raise ASDL.DecodeError
	      else Byte.bytesToString bytes
	  end

    fun writeIdentifier (outS, id) = writeString (outS, Atom.toString id)

    fun readIdentifier inS =  Atom.atom(readString inS)

  (* utility functions for sum-type tags *)
    fun writeTag8 (outS, tag) = BinIO.output1(outS, toByte tag)
    val readTag8 = readByte
    fun writeTag16 (outS, tag) = (
	  BinIO.output1(outS, toByte(tag >> 0w8));
	  BinIO.output1(outS, toByte tag));
    fun readTag16 inS = ((readByte inS) << 0w8) ++ (readByte inS)

    fun toFile write (file, x) = let
	  val outS = BinIO.openOut file
	  in
	    write (outS, x) handle exn => (BinIO.closeOut outS; raise exn);
	    BinIO.closeOut outS
	  end

    fun fromFile read file = let
	  val inS = BinIO.openIn file
	  in
	    (read inS handle exn => (BinIO.closeIn inS; raise exn))
	    before BinIO.closeIn inS
	  end

  end
