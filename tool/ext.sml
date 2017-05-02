(* ext.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Register the ASDL classifier
 *)

structure ASDLExt =
  struct

    val _ = Tools.registerClassifier (Tools.stdSfxClassifier { sfx = "asdl", class = "asdl" })

  end
