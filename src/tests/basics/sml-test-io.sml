(* sml-test-io.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure TestIO =
  struct

    local
      structure Pkl = TestSpecPickleIO
    (* pickle/unpickle identity *)
      fun ident (pickle, unpickle) x = let
	    val file = OS.FileSys.tmpName()
	    val outS = BinIO.openOut file
	    val _ = pickle (outS, x) handle exn => (BinIO.closeOut outS; raise exn)
	    val _ = BinIO.closeOut outS
	    val inS = BinIO.openIn file
	    val y = unpickle inS handle exn => (BinIO.closeIn inS; raise exn)
	    val _ = BinIO.closeIn inS
	    in
	      OS.FileSys.remove file;
	      y
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
