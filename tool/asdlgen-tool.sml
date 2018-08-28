(* asdlgen-tool.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLGenTool : sig end =
  struct

  (* given a file `foo.asdl`, we generate five files:
   *
   *	foo.sml			-- contains the generated type definitions
   *    foo-pickle.sig		-- the signature for the memory-pickling module
   *	foo-pickle.sml		-- the implementation of the memory-pickling operations
   *    foo-pickle-io.sig	-- the signature for the file-pickling module
   *	foo-pickleio.sml	-- the implementation of the file-pickling operations
   *)
    fun genFiles base = let
	  fun join ext = (base ^ ext, SOME "sml", Fn.id)
	  in
	    List.map join [
		".sml",
		"-pickle.sig", "-pickle.sml",
		"-pickle-io.sig", "-pickle-io.sml"
	      ]
	  end

    val _ = Tools.registerStdShellCmdTool {
	      tool = "ASDLGen",
	      class = "asdlgen",
	      cmdStdPath = fn () => ("asdlgen", ["sml"]),
	      template = NONE,
	      extensionStyle = Tools.RENAME(["asdl"], genFiles),
	      dflopts = []
	    }

  end
