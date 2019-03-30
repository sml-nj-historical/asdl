(* gen-memory-pickle.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the memory pickling code for the SML view
 *)

structure GenMemoryPickle =
    GenPickleFn (val getPklModName = SMLView.Module.getPickleName)
