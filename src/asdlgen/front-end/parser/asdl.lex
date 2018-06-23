(* asdl.lex
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * ML-ULex specification for ASDL
 *)

%name ASDLLex;

%arg (lexErr);

%defs(
  structure T = ASDLTokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* eof : unit -> lex_result *)
  (* ml-ulex requires this as well *)
    fun eof () = T.EOF

    val code = ref ([] : string list)
    fun addString s = code := s :: (!code)
    fun makeCode () = (T.CODE(concat(rev(!code))) before code := nil)

);

%states INITIAL CODELN CODE;

%let alpha = [A-Za-z];
%let alpha_num = [_0-9A-Za-z];
%let uc_id = [A-Z]{alpha_num}*;
%let lc_id = [a-z]{alpha_num}*;
%let ws = " "|[\t\n\v\f\r];
%let eol = [\n\r];

<INITIAL>{ws}+		=> (skip());
<INITIAL>"--#line"{ws}+[0-9]+({ws}+\"[^\"]*\")?
			=> ((* FIXME: resynch the sourcemap *) skip());
<INITIAL>"--"[^\n\r]*	=> (skip());
<INITIAL>"("		=> (T.LPAREN);
<INITIAL>")"		=> (T.RPAREN);
<INITIAL>"["		=> (T.LBRACK);
<INITIAL>"]"		=> (T.RBRACK);
<INITIAL>"{"		=> (T.LBRACE);
<INITIAL>"}"		=> (T.RBRACE);
<INITIAL>"<Top>"	=> (T.TOP);
<INITIAL>"<="		=> (T.LEQ);
<INITIAL>","		=> (T.COMMA);
<INITIAL>"*"		=> (T.SEQUENCE);
<INITIAL>"."		=> (T.DOT);
<INITIAL>"?"		=> (T.OPTIONAL);
<INITIAL>"!"		=> (T.SHARED);
<INITIAL>"|"		=> (T.PIPE);
<INITIAL>"="		=> (T.EQ);
<INITIAL>"attributes"	=> (T.KW_attributes);
<INITIAL>"module"	=> (T.KW_module);
<INITIAL>"primitive"	=> (T.KW_primitive);
<INITIAL>"imports"	=> (T.KW_imports);
<INITIAL>"alias"	=> (T.KW_alias);
<INITIAL>"view"		=> (T.KW_view);
<INITIAL>{uc_id}	=> (T.UID(Atom.atom yytext));
<INITIAL>{lc_id}	=> (T.LID(Atom.atom yytext));
<INITIAL>":"[ \t]*      => (YYBEGIN CODELN; continue());
<CODELN>[^\n\r]*	=> (YYBEGIN INITIAL; T.CODE yytext);
<INITIAL>"%%"{ws}*	=> (YYBEGIN CODE; continue());
<CODE>"%%"		=> (YYBEGIN INITIAL; makeCode());
<CODE>.*		=> (addString(yytext); continue());

<INITIAL>.              => (lexErr(yypos, ["bad character `", String.toString yytext, "'"]);
                            continue());
