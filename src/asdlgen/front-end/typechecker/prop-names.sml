(* prop-names.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The view-property names.
 *)

structure PropNames =
  struct

    val encoder = Atom.atom "encoder"
    val decoder = Atom.atom "decoder"
    val reader = Atom.atom "reader"
    val writer = Atom.atom "writer"
    val name = Atom.atom "name"
    val pickler_name = Atom.atom "pickler_name"
    val io_name = Atom.atom "io_name"
    val header = Atom.atom "header"
    val natural_type = Atom.atom "natural_type"

    val base_type = Atom.atom "base_type"
    val public_code = Atom.atom "public_code"
    val protected_code = Atom.atom "protected_code"
    val private_code = Atom.atom "private_code"
    val enum_value = Atom.atom "enum_value"

  end


