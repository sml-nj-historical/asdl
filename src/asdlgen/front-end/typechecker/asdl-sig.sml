(* asdl-sig.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ASDL =
  sig

  (* ASDL modules *)
    structure Module : sig
	type t

      (* get the name of the module *)
	val nameOf : t -> string

      (* is the module primitive? *)
	val isPrim : t -> bool

      end

  (* ASDL named types *)
    structure Type : sig
	type t

      (* get the name of the type *)
	val nameOf : t -> string

      (* get the module that this type belongs to *)
	val moduleOf : t -> Module.t
      end

    structure Con : sig
	type t

      (* get the name of the constructor *)
	val nameOf : t -> string

      (* get the type that this constructor belongs to *)
	val ownerOf : t -> Type.t

      (* get the module that this constructor belongs to *)
	val moduleOf : t -> Module.t
      end

    structure Field : sig
	type t

      (* get the name of the field *)
	val nameOf : t -> string option

      end

    type spec = Module.t list

  end



