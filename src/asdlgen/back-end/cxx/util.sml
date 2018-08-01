(* util.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Util : sig

  (* is a type an enumeration? *)
    val isEnum : AST.TypeId.t -> bool

  (* is a type represented by a pointer (i.e., boxed) or by an immediate value (unboxed)? *)
    val isBoxed : AST.TypeId.t -> bool

  (* names of components for tuples and records *)
    val posName : int -> string
    val fieldName : string -> string

  end = struct

    fun isEnum tyId = (case AST.TypeId.bindingOf tyId
	   of (AST.TyDcl{def = ref(AST.EnumTy _), ...}) => true
	    | _ => false
	  (* end case *))

    fun isBoxed tyId = not(isEnum tyId)
	  andalso ??

    fun posName i = "_v" ^ Int.toString i
    fun fieldName lab = "_" ^ lab

  end


