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

  (* parse the command-line args *)
    val parseCmdLine : string list -> {
            help : bool,                (* "-h" or "--help" specified? *)
            version : bool,             (* "--version" specified? *)
            about : bool,               (* "--about" specified? *)
	    command : string option,	(* the specified command *)
	    pickler : string option,	(* the specified pickler *)
            files : string list         (* source file *)
          }

  (* return a usage message.  The boolean controls whether all options should be
   * included (true ==> long; false ==> short).
   *)
    val usage : string * bool -> string

  end = struct

    structure G = GetOpt
    structure P = OS.Path

    exception Usage of string

  (* option flags that are set by getOpt *)
    val viewOpt : string option ref = ref NONE
    val picklerOpt : string option ref = ref NONE

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

    fun parse (cmd, [])) = {
            help = SOME false,
            version = false,
	    command = cmd,
            files = []
          }
      | parse args = let
	  val (opts, files) = G.getOpt {
		  argOrder = G.RequireOrder,
		  options = optionList @ ctlOptions,
		  errFn = fn s => raise Usage s
		} rest
	(* figure out filename pieces *)
	  val srcFiles =
		if isSome(!helpFlg) orelse !versionFlg orelse !aboutFlg
		  then []
		  else (case files
		     of [] => raise Usage "missing file argument"
		      | fs => fs
		    (* end case *))
	  in {
	    help = !helpFlg,
	    version = !versionFlg,
	    command = cmd,
	    file = srcFile
	  } end

    val commands = [
	    "help",
	    "version",
	    "c++", "cxx",
	    "sml",
	    "typ",
	    "check"
	  ]

    fun parseCmdLine (cmd::rest) =
	  if List.exists (fn cmd' => (cmd = cmd')) commands
	    then parse (cmd, rest)
	    else raise Usage "unknown command"

    fun usage (cmd, long) = let
          val hdr = concat[
                  "usage: ", cmd, " command [options] file ...\n",
                  "  Version: ", Version.message, "\n",
                  "  Commands:",
(* FIXME: add commands *)
                  "  Options:"
                ]
          in
            G.usageInfo {header = hdr, options = options}
          end

  end
