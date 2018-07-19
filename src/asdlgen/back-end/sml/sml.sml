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
      = STRspec of {id : id, sign : sign, def : strexp}
      | TYCspec of {entVar : EntPath.entVar, info: tycSpecInfo}
      | VALspec of id * ty
      | EXNspec of id * ty option

    and dec
      = VALdec of vb list
      | FUNdec of rvb list
      | TYPEdec of Types.tycon list
      | DATATYPEdec of {datatycs: Types.tycon list, withtycs: Types.tycon list}
      | EXCEPTIONdec of eb list
      | STRdec of strb list
      | OPENdec of (SymPath.path * Modules.Structure) list
      | LOCALdec of dec * dec
      | VERBdec of string list

    and exp
      = VARexp of id
      | CONexp of VarCon.datacon * Types.tyvar list (* instance type *)
      | NUMexp of string * num_lit	(* string is source text of literal *)
      | REALexp of string * real_lit	(* string is source text of literal *)
      | STRINGexp of string
      | CHARexp of string
      | RECORDexp of (numberedLabel * exp) list
      | SELECTexp of numberedLabel * exp
      | VECTORexp of exp list * Types.ty
      | APPexp of exp * exp
      | HANDLEexp of exp * fnrules
      | RAISEexp of exp * Types.ty
      | CASEexp of exp * rule list * bool
      | IFexp of { test: exp, thenCase: exp, elseCase: exp }
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

  end


