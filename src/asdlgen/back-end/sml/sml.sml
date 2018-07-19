(* sml.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Syntax trees for a subset of SML.  Used to generate SML code.
 *)

structure SML =
  struct

    type id = string

    datatype top_decl
      = SIGtop of id * sigbody
      | STRtop of id * sign option * strexp
      | VERBtop of string list

    and sign
      = IDsig of id
      | SIGsig of sigbody

    and sigbody = SIG of spec list * where_ty list

    and strexp
      = IDstr of id
      | STRstr of dec list
      | VERBstr of string list

    and spec
      = STRspec of id * sign
      | TYCspec of bool * id list * id * ty option
      | VALspec of id * ty
      | EXNspec of id * ty option

    and dec
      = VALdec of pat * exp
      | FUNdec of fb list
      | TYPEdec of id list * id * ty
      | DATATYPEdec of db list * (id list * id * ty) list
      | EXCEPTIONdec of eb list
      | STRdec of strb list
      | OPENdec of id list
      | LOCALdec of dec list * dec list
      | VERBdec of string list

  (* function binding *)
    and fb
      = FB of (id * (pat list * exp) list)

  (* datatype binding *)
    and db
      = DB of id list * id * (id * ty option) list

    and exp
      = IDexp of id			(* variables and constuctors *)
      | NUMexp of string
      | STRINGexp of string
      | CHARexp of string
      | RECORDexp of (id * exp) list
      | SELECTexp of id * exp
      | VECTORexp of exp list * Types.ty
      | APPexp of exp * exp
      | HANDLEexp of exp * fnrules
      | RAISEexp of exp * Types.ty
      | CASEexp of exp * rule list * bool
      | IFexp of exp * exp * exp
      | ANDALSOexp of exp * exp
      | ORELSEexp of exp * exp
      | FNexp of fnrules
      | LETexp of dec * exp
      | SEQexp of exp list
      | CONSTRAINTexp of exp * Types.ty
      | VERBexp of string

    and rule = RULE of pat * exp

    and pat
      = WILDpat
      | VARpat of VarCon.var
      | NUMpat of string * num_lit	(* string is source text of literal *)
      | STRINGpat of string
      | CHARpat of string
      | CONpat of VarCon.datacon * Types.tyvar list (* instance type *)
      | RECORDpat of {fields : (Types.label * pat) list,
		      flex : bool, typ : Types.ty ref}
      | APPpat of VarCon.datacon * Types.tyvar list * pat
      | CONSTRAINTpat of pat * Types.ty
      | LAYEREDpat of pat * pat

    and ty
      = VARty of id			(* type variable *)
      | CONty of ty list * id		(* type constructor *)
      | RECORDty of (id * ty) list 	(* record *)
      | TUPLEty of ty list		(* tuple *)
      | VERBty of string		(* verbatim type expression *)

  end


