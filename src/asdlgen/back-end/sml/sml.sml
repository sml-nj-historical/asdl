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
      = SIGtop of id * sigexp
      | STRtop of id * (bool * sigexp) option * strexp
      | VERBtop of string list

    and sigexp
      = IDsig of id				(* signature variable *)
      | AUGsig of sigexp * where_ty list	(* sig augmented with where specs *)
      | BASEsig of spec list			(* basic signature (sig...end) *)

    and where_ty
      = WHERETY of id list * id list * ty

    and strexp
      = IDstr of id
      | BASEstr of dec list
      | VERBstr of string list

    and spec
      = STRspec of id * sigexp
      | TYPEspec of bool * id list * id * ty option
      | DATATYPEspec of db list
      | VALspec of id * ty
      | EXNspec of id * ty option

    and dec
      = VALdec of pat * exp
      | FUNdec of fb list
      | TYPEdec of id list * id * ty
      | DATATYPEdec of db list * (id list * id * ty) list
      | EXCEPTIONdec of id * ty option
      | STRdec of id * sigexp option * strexp
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
      | TUPLEexp of exp list
      | SELECTexp of id * exp
      | APPexp of exp * exp
      | HANDLEexp of exp * (pat * exp) list
      | RAISEexp of exp
      | CASEexp of exp * (pat * exp) list
      | IFexp of exp * exp * exp
      | ANDALSOexp of exp * exp
      | ORELSEexp of exp * exp
      | FNexp of (pat * exp) list
      | LETexp of dec list * exp
      | SEQexp of exp list
      | CONSTRAINTexp of exp * ty
      | VERBexp of string

    and pat
      = WILDpat
      | IDpat of id
      | NUMpat of string
      | STRINGpat of string
      | CHARpat of string
      | CONpat of id * pat
      | RECORDpat of {fields : (id * pat) list, flex : bool}
      | TUPLEpat of pat list
      | CONSTRAINTpat of pat * ty
      | ASpat of id * pat

    and ty
      = VARty of id			(* type variable *)
      | CONty of ty list * id		(* type constructor *)
      | FUNty of ty * ty		(* function type *)
      | RECORDty of (id * ty) list 	(* record *)
      | TUPLEty of ty list		(* tuple *)
      | VERBty of string		(* verbatim type expression *)

  (* construct a simple function binding of the form `f (x1, ..., xn) = e` *)
    fun simpleFB (f, [x], e) = FB(f, [([IDpat x], e)])
      | simpleFB (f, xs, e) = FB(f, [([TUPLEpat(List.map IDpat xs)], e)])

    fun tupleTy [] = CONty([], "unit")
      | tupleTy [ty] = ty
      | tupleTy tys = TUPLEty tys

    fun tupleExp [e] = e
      | tupleExp es = TUPLEexp es

    fun tuplePat [p] = p
      | tuplePat ps = TUPLEpat ps

  end


