(* gen-file-pickle.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the file pickling code for the SML view
 *
 * TODO: merge with gen-pickle.sml
 *)

structure GenFilePickle =
    GenPickleFn (val getPklModName = SMLView.Module.getIOName)
