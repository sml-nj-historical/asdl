(* parse-tree.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Parse tree representation of ASDL specification
 *)

structure ParseTree =
  struct

    type 'a mark = 'a Error.mark

    type id = Atom.atom mark

    val topId = Atom.atom "<top>"

    datatype decl
      = D_Mark of decl mark
      | D_Module of {
	    name : id,
	    imports : import list,
	    decls : type_decl list
	  }
      | D_Primitive of {
	    name : id,
	    exports : id list
	  }
      | D_View of {
	    name : id,
	    decls : view_decl list
	  }

    and import
      = Import_Mark of import mark
      | Import of {
	    module : id, 
            alias : id option
	  }

    and type_decl
      = TD_Mark of type_decl mark
      | TD_Sum of {
	    name : id,
	    attribs : field list,
	    cons : cons list
	  }
      | TD_Product of {
	    name : id,
	    fields : field list
	  }

    and field
      = Field_Mark of field mark
      | Field of {
	    module : id option,		(* optional module qualifier *)
	    typ : id,			(* type of field *)
	    tycon : tycon option,	(* type operator *)
	    label : id option		(* optional field label *)
	  }

    and cons
      = Cons_Mark of cons mark
      | Cons of id * field list

    and tycon = Optional | Sequence | Shared

    and view_decl
      = ViewDcl_Mark of view_decl option
      | ViewDcl of {
	    entity : id list,
	    property : id,
	    value : string mark
	  }

  end
