(* gen-cxx.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the C++ view of the ASDL modules.
 *)

structure GenCxx : sig

    val gen : {src : string, dir : string, stem : string, modules : AST.module list} -> unit

  end = struct

    structure Cons = ASDL.Constr
    structure CL = CLang

    type code = {
	hxx : CL.decl list,
	cxx : CL.decl list
      }

  (* include directives to include in the .hxx and .cxx files *)
    val hxxIncls = CL.D_Verbatim[
	    "#include \"asdl/asdl.hxx\"\n",
	  ]
    val cxxIncls = CL.D_Verbatim[
	    "#include \"@HXX_FILENAME@\"\n",
	  ]

  (* generate code for a constructor *)
    fun defConstr view tyName (cons, cd : code) = let

	(* the class definition for the constructor *)
	  val cls = CL.D_ClassDef{
		  name = Cons.nameOf(view, cons),
		  args = NONE,
		  from = SOME tyName,
		  public = ??,
		  protected = [],
		  private = fields
		}
	  in
	    {cls :: #hxx cd, fns @ #cxx cd}
	  end

  (* generate C++ code for the given list of modules using the "Cxx" view *)
    fun gen {src, dir, stem, modules} = let
	  val basePath = OS.Path.joinDirFile{dir=dir, file=stem}
	  fun cxxFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "cxx"}
	  fun hxxFilename name = OS.Path.joinBaseExt{base=name, ext=SOME "hxx"}
	  in
	  end

  end
