(* asdl-sexp-pickle.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ASDLSExpPickle : sig

    type instream = TextIO.instream
    type outstream = TextIO.outstream

    val writeBool : outstream * ASDL.bool -> unit
    val readBool : instream -> ASDL.bool

    val writeInt : outstream * ASDL.int -> unit
    val readInt : instream -> ASDL.int

    val writeUInt : outstream * ASDL.uint -> unit
    val readUInt : instream -> ASDL.uint

    val writeInteger : outstream * ASDL.integer -> unit
    val readInteger : instream -> ASDL.integer

    val writeString : outstream * ASDL.string -> unit
    val readString : instream -> ASDL.string

    val writeIdentifier : outstream * ASDL.identifier -> unit
    val readIdentifier : instream -> ASDL.identifier

  (* pickle to/from files *)
    val toFile : (outstream * 'a -> unit) -> (string * 'a) -> unit
    val fromFile : (instream -> 'a) -> string -> 'a

  end  = struct

(* TODO: add pretty-printing support *)

    type instream = TextIO.instream
    type outstream = TextIO.outstream

    fun writeBool (outS, true) = TextIO.output(outS, "#t")
      | writeBool (outS, false) = TextIO.output(outS, "#f")

    fun readBool inS = raise Fail "unimplemented"

    fun writeInt (outS, n) = if n < 0
	  then TextIO.output(outS, "-" ^ Int.toString(~n))
	  else TextIO.output(outS, Int.toString n)

    fun readInt inS = raise Fail "unimplemented"

    fun writeUInt (outS, n) = TextIO.output(outS, Word.fmt StringCvt.DEC n)

    fun readUInt inS = raise Fail "unimplemented"

    fun writeInteger (outS, n) = if n < 0
	  then TextIO.output(outS, "-" ^ IntInf.toString(~n))
	  else TextIO.output(outS, IntInf.toString n)

    fun readInteger inS = raise Fail "unimplemented"

(* FIXME: what is the correct escape encoding? *)
    fun writeString (outS, s) =
	  TextIO.output(outS, String.concat["\"", String.toCString s, "\""])

    fun readString inS = raise Fail "unimplemented"

    fun writeIdentifier (outS, id) = writeString (outS, Atom.toString id)

    fun readIdentifier inS = Atom.atom(readString inS)

    fun toFile write (file, x) = let
	  val outS = TextIO.openOut file
	  in
	    write (outS, x) handle exn => (TextIO.closeOut outS; raise exn);
	    TextIO.closeOut outS
	  end

    fun fromFile read file = let
	  val inS = TextIO.openIn file
	  in
	    (read inS handle exn => (TextIO.closeIn inS; raise exn))
	    before TextIO.closeIn inS
	  end

  end (* structure ASDLSExpPickle *)
