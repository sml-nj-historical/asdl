(* sml-test-mix.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Tests interaction between memory and file pickling.
 *)

structure TestMix =
  struct

    local
      open TestSpec
      structure Pkl = TestSpecPickle
      structure PIO = TestSpecPickleIO
      structure U = Util
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
    (* pickle by first converting to bytes and then writing the bytes *)
      fun wrapPkl pick (outS, x) = BinIO.output(outS, ASDLPickle.toVector pick x)
      fun wrapUnkpkl unpick inS = ASDLPickle.fromVector unpick (BinIO.inputAll inS)
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
      fun checkFM name (toStr, same, wr, unpick) = check ("FM "^name) (toStr, same, wr, wrapUnkpkl unpick)
      fun checkMF name (toStr, same, pick, rd) = check ("MF "^name) (toStr, same, wrapPkl pick, rd)
    in
  (* tree: file -> memory *)
    fun chkTreeFM () = let
	  val chk = checkFM "tree" (U.tree_toString, U.tree_same, PIO.write_tree, Pkl.decode_tree)
	  in
	    chk EMPTY;
	    chk (NODE{value ="2", left=NODE{value ="1", left = EMPTY, right = EMPTY}, right=EMPTY});
	    chk (NODE{value ="1", left=EMPTY, right=NODE{value ="2", left = EMPTY, right = EMPTY}});
	    chk (NODE{
		value ="2",
		left=NODE{value ="1", left = EMPTY, right = EMPTY},
		right=NODE{value ="3", left = EMPTY, right = EMPTY}})
	  end
  (* tree: memory -> file *)
  (* coord *)
    fun chkCoordFM () = let
	  val chk = checkFM "coord" (U.coord_toString, U.coord_same, PIO.write_coord, Pkl.decode_coord)
	  in
	    chk {x = 12, y = 13};
	    chk {x = ~12, y = 13}
	  end
  (* pos *)
    fun chkPosFM () = let
	  val chk = checkFM "pos" (U.pos_toString, U.pos_same, PIO.write_pos, Pkl.decode_pos)
	  in
	    chk (12, 13);
	    chk (~12, 42)
	  end
  (* nat *)
    fun chkNatFM () = let
	  val chk = checkFM "nat" (U.nat_toString, U.nat_same, PIO.write_nat, Pkl.decode_nat)
	  in
	    chk ZERO;
	    chk (SUCC ZERO);
	    chk (SUCC(SUCC(SUCC ZERO)))
	  end
  (* value *)
    fun chkValueFM () = let
	  val chk = checkFM "value" (U.value_toString, U.value_same, PIO.write_value, Pkl.decode_value)
	  in
	    chk (BOOL false);
	    chk (BOOL true);
	    chk (INT ~1);
	    chk (INT 0);
	    chk (INT 1);
	    chk (INT 42);
	    chk (STRING "");
	    chk (STRING "a");
	    chk (STRING "abc\n")
	  end
  (* color *)
    fun chkColorFM () = let
	  val chk = checkFM "color" (U.color_toString, U.color_same, PIO.write_color, Pkl.decode_color)
	  in
	    chk RED;
	    chk GREEN;
	    chk BLUE
	  end
  (* wrap_bool *)
    fun chkWrapBoolFM () = let
	  val chk = checkFM "wrap_bool" (U.wrap_bool_toString, U.wrap_bool_same, PIO.write_wrap_bool, Pkl.decode_wrap_bool)
	  in
	    chk (WRAP true);
	    chk (WRAP false)
	  end
  (* unit *)
    fun chkUnitFM () = let
	  val chk = checkFM "unit" (U.unit_toString, U.unit_same, PIO.write_unit, Pkl.decode_unit)
	  in
	    chk UNIT
	  end
    end (* local *)

    fun chkAll () = (
	  chkUnitFM();
	  chkWrapBoolFM();
	  chkColorFM();
	  chkValueFM();
	  chkNatFM();
	  chkPosFM();
	  chkCoordFM();
	  chkTreeFM())

  end
