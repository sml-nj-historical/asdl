(* view.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure View : sig

    type t

    datatype entity
      = Module of AST.ModuleId.t
      | Type of AST.TypeId.t
      | Constr of AST.ConsId.t

    structure Prop : sig
	type t
      end

    val nameOf : t -> string

  (* find the property instance for the given view, entity, and property name *)
    val findProp : t * entity * Atom.atom -> Prop.t option

  (* a view template describes the properties that the various entities of a module
   * might have.
   *)
    type template = {
	moduleProps : prop_desc ATbl.hash_table,
	typeProps : prop_desc ATbl.hash_table,
	consProps : prop_desc ATbl.hash_table
      }

  (* create a new view with the given name and properties *)
    val new : string * template -> t

  end = struct

    structure ATbl = AtomTable

    datatype entity
      = Module of AST.ModuleId.t
      | Type of AST.TypeId.t
      | Constr of AST.ConsId.t

  (* hash table on entities *)
    structure ETbl = HashTableFn (
      struct
	type hash_key = entity
	fun hash (Module id) = Word.<<(AST.ModuleId.hash id, 0w2) + 0w1
	  | hash (Type id) = Word.<<(AST.TypeId.hash id, 0w2) + 0w2
	  | hash (Constr id) = Word.<<(AST.ConsId.hash id, 0w2) + 0w3
	fun same (Module id1, Module id2) = AST.ModuleId.same(id1, id2)
	  | same (Type id1, Type id2) = AST.TypeId.same(id1, id2)
	  | same (Constr id1, Cons id2) = AST.ConsId.same(id1, id2)
	  | same _ = false
      end)

    datatype prop_desc = PDesc of {
	name : Atom.atom,		(* the property's name *)
	accumulator : bool		(* true if values are cumulative *)
      }

    datatype prop = Prop of {
	name : Atom.atom,
	entity : entity,
	accumulator : bool,		(* true if values are cumulative *)
	value : string list ref
      }

    structure Prop =
      struct
	type t = prop
	fun nameOf (Prop{name, ...}) = name
      (* create a new property instance for an entity *)
	fun new (PDesc{name, accumulator}, entity) = Prop{
		name = name,
		entity = entity,
		accumulator = accumulator,
		value = ref[]
	      }
      end

  (* table to map entities to their property tables *)
    type entity_tbl = Prop.prop ATbl.hash_table ETbl.hash_table

  (* a view template describes the properties that the various entities of a module
   * might have.
   *)
    type template = {
	moduleProps : prop_desc ATbl.hash_table,
	typeProps : prop_desc ATbl.hash_table,
	consProps : prop_desc ATbl.hash_table
      }

  (* find the property descriptor for a given entity and property name *)
    fun findPropDesc ({moduleProps, typeProps, consProps}, entity, prop) = (
	  case entity
	   of Module _ => ATbl.find moduleProps prop
	    | Type _ => ATbl.find typeProps prop
	    | Constr _ => ATbl.find consProps prop
	  (* end case *))

    type t = View of {
	name : string,		(* the view's name *)
	template : template,	(* template that specifies which properties are supported by
				 * the view *)
	eTbl : entity_tbl	(* table to map entities to their property tables *)
      }

  (* create a new view *)
    fun new (name, template) = View{
	    name = name,
	    template = template,
	    eTbl = ETbl.mkTable (64, Fail "view-entity table")
	  }

    fun nameOf (View{name, ...}) = name

  (* find a property instance; we create a new instance from the template if necessary *)
    fun findProp (View{eTbl, template, ...}, entity, name) = (case ETbl.find eTbl entity
	   of SOME pTbl => ATbl.find pTbl name
	    | NONE => (case findPropDesc (template, entity, name)
		 of SOME pdesc => let
		      val prop = Prop.new (entity, pdesc)
		      in
			ATbl.insert pTbl (name, prop);
			SOME prop
		      end
		  | NONE => NONE
		(* end case *))
	  (* end case *))

  end
