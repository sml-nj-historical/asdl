(* options.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Options : sig

  (* raised if parsing command-line args hits an error (e.g., missing option, syntax, ...).
   * The string is an error message.
   *)
    exception Usage of string

    type generator = {
	src : string,			(* name of source file from command line *)
	dir : string,			(* output directory for generated files *)
	stem : string,			(* stem of output files *)
	modules : AST.module list	(* modules from source file *)
      } -> unit

    datatype command
      = HELP
      | VERSION
      | CHECK
      | GENERATE of generator

  (* register the generators *)
    val registerGen : string list * generator -> unit

  (* parse the command-line args *)
    val parseCmdLine : string list -> {
	    command : command,		(* the first command-line argument, which specifies the
					 * operation to perform *)
            files : string list         (* source file *)
          }

  (* return a usage message. *)
    val usage : unit -> string

  (* get option values *)
    val lineWidth : unit -> int			(* set by `--line-width` *)
    val outputDir : unit -> string option	(* set by `-d` / `--output-directory` *)
    val pickler : unit -> string		(* set by `--pickler` *)

  end = struct

    structure G = GetOpt
    structure P = OS.Path

    type generator = {
	src : string,			(* name of source file from command line *)
	dir : string,			(* output directory for generated files *)
	stem : string,			(* stem of output files *)
	modules : AST.module list	(* modules from source file *)
      } -> unit

    datatype command
      = HELP
      | VERSION
      | CHECK
      | GENERATE of generator

    exception Usage of string

  (* option flags that are set by getOpt *)
    val viewOpt : string option ref = ref NONE
    val picklerOpt : string option ref = ref NONE
    val lineWidOpt : int ref = ref 90
    val outputDirOpt : string option ref = ref NONE

    fun setFlag (flg, value) = G.NoArg(fn () => (flg := value))

  (* the short list of options, which does not include the compiler controls *)
    val optionList = [
(* TODO:
-n
--line-width
--output-directory -d
*)
	    { short = "p", long = ["pickler"],
	      desc = G.ReqArg(fn s => picklerOpt := SOME s, "{binary,sexp,xml,empty}"),
	      help = "specify kind of pickler"
	    }
          ]

(* TODO: view-specific options *)

    fun parse (cmd, []) = {
	    command = cmd,
            files = []
          }
      | parse (cmd, args) = let
	  val (opts, files) = G.getOpt {
		  argOrder = G.RequireOrder,
		  options = optionList,
		  errFn = fn s => raise Usage s
		} args
	(* figure out filename pieces *)
	  val srcFiles = (case (cmd, files)
		 of (HELP, _) => []
		  | (VERSION, _) => []
		  | (_, []) => raise Usage "no input files specified"
		  | _ => files
		(* end case *))
	  in {
	    command = cmd,
	    files = srcFiles
	  } end

    val commands = ref [
	    (["help"],		HELP),
	    (["version"],	VERSION),
	    (["check"],		CHECK)
	  ]

    fun registerGen (names, genFn) = commands := (names, GENERATE genFn) :: !commands

    fun parseCmdLine (cmd::rest) = let
	  fun isCmd (names, _) = List.exists (fn name => (name = cmd)) names
	  in
	    case List.find isCmd (!commands)
	     of SOME(_, cmd) => parse (cmd, rest)
	      | NONE => raise Usage "unknown command"
	    (* end case *)
	  end

    fun usage () = let
          val hdr = concat[
                  "usage: asdlgen command [options] file ...\n",
                  "  Version: ", Config.version, "\n",
                  "  Commands:\n",
		  "    help\n",
		  "    version\n",
		  "    check\n",
(* FIXME: add commands *)
                  "  Options:"
                ]
          in
            G.usageInfo {header = hdr, options = optionList}
          end

  (* get option values *)
    fun lineWidth () = !lineWidOpt

    fun outputDir () = !outputDirOpt

    fun pickler () = Option.getOpt(!picklerOpt, "binary")

  end
