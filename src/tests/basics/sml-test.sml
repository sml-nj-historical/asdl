(* sml-test.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Test =
  struct

    local
      structure Pkl = TestSpecPickle
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
    end (* local *)

  end
