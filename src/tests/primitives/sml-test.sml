(* sml-test.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Test primitive pickling operations from the ASDL library.
 *)

structure Test =
  struct

    local
      structure Pkl = ASDLPickle
    (* pickle/unpickle identity *)
      fun ident (pickle, unpickle) x = let
	    val buf = Word8Buffer.new 100
	    val _ = pickle (buf, x)
	    val (y, rest) = unpickle (Word8VectorSlice.full(Word8Buffer.contents buf))
	    in
	      if Word8VectorSlice.length rest <> 0
		then raise Fail(concat[
		    Int.toString(Word8VectorSlice.length rest),
		    " excess bytes after unpickling"
		  ])
		else y
	    end
    (* check that the pickle/unpickle cycle preserves values *)
      fun check name (toStr, same, pick, unpick) x = let
	    val _ = print(concat["check ", name, ": unpickle(pickle ", toStr x, ")"])
	    val y = ident (pick, unpick) x
	    in
	      if same(x, y)
		then print " ok\n"
		else print(concat[" fail (", toStr y, ")\n"])
	    end
	      handle exn => print(concat[" fail(", exnMessage exn, ")\n"])
    in
  (* booleans *)
    fun chkBool () = let
	  val chk = check "boolean" (Bool.toString, op =, Pkl.encodeBool, Pkl.decodeBool)
	  in
	    chk true;
	    chk false
	  end
  (* int *)
    fun chkInt () = let
	  val chk = check "int" (Int.toString, op =, Pkl.encodeInt, Pkl.decodeInt)
	  in
	    chk 0;
	    chk ~1;
	    chk 1;
	    chk ~32;
	    chk ~31;
	    chk 31;
	    chk 32;
	    chk ~8192;
	    chk ~8191;
	    chk 8191;
	    chk 8192;
	    chk ~2097152;
	    chk ~2097151;
	    chk 2097151;
	    chk 2097152;
	    chk ~536870912;	(* lower bound *)
	    chk 536870911	(* upper bound *)
	  end
  (* uint *)
    fun chkUInt () = let
	  fun toS w = "0x" ^ Word.toString w
	  val chk = check "uint" (toS, op =, Pkl.encodeUInt, Pkl.decodeUInt)
	  in
	    chk 0w0;
	    chk 0w1;
	    chk 0wx3f;
	    chk 0wx100;
	    chk 0wx3fff;
	    chk 0wx10000;
	    chk 0wx3fffff;
	    chk 0wx1000000;
	    chk 0wx3fffffff	(* upper bound *)
	  end
  (* integer *)
    fun chkInteger () = let
	  val chk = check "integer"
		(IntInf.toString, op =, Pkl.encodeInteger, Pkl.decodeInteger)
	  in
	    chk 0;
	    chk ~1;
	    chk 1;
	    chk ~64;
	    chk ~63;
	    chk 63;
	    chk 64;
	    chk ~8192;
	    chk ~8191;
	    chk 8191;
	    chk 8192;
	    chk ~2097152;
	    chk ~2097151;
	    chk 2097151;
	    chk 2097152;
	    chk ~36893488147419103232;	(* -2^65 *)
	    chk 36893488147419103232;	(* 2^65 *)
	    chk 73786976294838206463	(* 2^66-1 *)
	  end
  (* string *)
    fun chkString () = let
	  fun toS s = String.concat["\"", String.toString s, "\""]
	  val chk = check "string" (toS, op =, Pkl.encodeString, Pkl.decodeString)
	  in
	    chk "";
	    chk " ";
	    chk "hello world\n"
	  end
  (* identifier *)
    fun chkIdentifier () = let
	  fun toS s = String.concat["\"", String.toString(Atom.toString s), "\""]
	  val chk = check "identifier"
		(toS, Atom.same, Pkl.encodeIdentifier, Pkl.decodeIdentifier)
	  in
	    chk (Atom.atom "");
	    chk (Atom.atom "x");
	    chk (Atom.atom "x1");
	    chk (Atom.atom "hello world\n")
	  end
  (* tag8 *)
    fun chkTag8 () = let
	  fun toS w = "0x" ^ Word.toString w
	  val chk = check "tag8" (toS, op =, Pkl.encodeTag8, Pkl.decodeTag8)
	  in
	    chk 0w0;
	    chk 0w1;
	    chk 0w17;
	    chk 0w255		(* upper bound *)
	  end
  (* tag16 *)
    fun chkTag16 ()= let
	  fun toS w = "0x" ^ Word.toString w
	  val chk = check "tag8" (toS, op =, Pkl.encodeTag16, Pkl.decodeTag16)
	  in
	    chk 0w0;
	    chk 0w1;
	    chk 0w17;
	    chk 0w255;
	    chk 0w256;
	    chk 0w65535		(* upper bound *)
	  end
    end (* local *)

  (* check all primitive types *)
    fun chkAll () = (
	  chkBool ();
	  chkInt ();
	  chkUInt ();
	  chkInteger ();
	  chkString ();
	  chkIdentifier ();
	  chkTag8 ();
	  chkTag16 ())

  (* functions to support interactive debugging *)
    local
      structure Pkl = ASDLPickle
      fun toBytes pickle x = let
	    val buf = Word8Buffer.new 100
	    in
	      pickle (buf, x);
	      Word8Vector.toList(Word8Buffer.contents buf)
	    end
    in
    val intToBytes = toBytes Pkl.encodeInt
    val uintToBytes = toBytes Pkl.encodeUInt
    end (* local *)

  end
