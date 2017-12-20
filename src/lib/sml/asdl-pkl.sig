(* asdl-pkl.sig
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature ASDL_PKL =
  sig

(* TODO: what about pickling to memory ? *)
    type instream = BinIO.instream
    type outstream = BinIO.outstream

    exception IOError of string

    val writeTag    : Int.int -> outstream -> unit
    val readTag     : instream -> Int.int

    val writeList   : ('a -> outstream -> unit) -> 'a list -> outstream -> unit
    val readList    : (instream -> 'a) -> instream -> 'a list

    val writeVec    : ('a -> outstream -> unit) -> 'a list -> outstream -> unit
    val readVec     : (instream -> 'a) -> instream -> 'a list

    val writeOption : ('a -> outstream -> unit) -> 'a option -> outstream -> unit
    val readOption  : (instream -> 'a) -> instream -> 'a option

    val readShare   : (instream -> 'a) -> instream -> 'a Share.share
    val writeShare  : ('a -> outstream -> unit) -> 'a Share.share -> outstream -> unit

  end
