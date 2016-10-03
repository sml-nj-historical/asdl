(* parser.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Parser :> sig

    val parse : Error.err_stream * string list -> {file : string, decl : ASDL.decl} list

  end = struct

  (* error function for lexers *)
    fun lexErr errStrm (pos, msg) = Error.errorAt(errStrm, (pos, pos), msg)

    local
    (* map tokens to strings; when adding a token, we use a generic name where it makes sense *)
      fun tokToString ADD (ASDLTokens.LID _) = "<lower-case identifier>"
	| tokToString DEL (ASDLTokens.LID x) = Atom.toString x
	| tokToString ADD (ASTLTokens.UID _) = "<upper-case identifier>"
	| tokToString DEL (ASDLTokens.UID x) = Atom.toString x
	| tokToString _ (ASDLTokens.CODE _) = "<code>"
	| tokToString _ tok = ASDLTokens.toString tok

    (* error function for parsers *)
      val parseErr = Error.parseError tokToString

    (* glue together the lexer and parser *)
      structure ASDLParser = ASDLParseFn(ASDLLex)
    in
    fun parser (errStrm, files) = let
          fun get () = TextIO.input file
	  val lexer = ASDLLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
	  val (res, _, errs) = ASDLParser.parse lexer (ASDLLex.streamify get)
	  in
	    List.app (parseErr errStrm) errs;
	    res
	  end
    end (* local *)

  end
