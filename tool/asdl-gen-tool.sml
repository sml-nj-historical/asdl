(* asdl-gen-tool.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLGenTool =
  struct

    val _ = Tools.registerStdShellCmdTool {
	      tool = "ASDLGen",
	      class = "asdl",
	      cmdStdPath = fn () => ("asdl-gen", ["--sml"]),
	      template = NONE,
	      extensionStyle =
	        Tools.EXTEND [("sml", SOME "sml", fn too => too)],
	      dflopts = []
	    }

  end
