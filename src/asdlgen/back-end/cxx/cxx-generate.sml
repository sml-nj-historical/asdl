(* cxx-generate.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CxxGenerate : sig

    val gen : ?? -> ??

  end = struct

    structure Cons = ASDL.Constr
    structure CL = CLang

    type code = {
	hxx : CL.decl list,
	cxx : CL.decl list
      }

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

  end
