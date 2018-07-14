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
      | Constr of AST.ConId.t

    structure Prop : sig
	type t

	datatype desc = Desc of {
	    name : Atom.atom,		(* the property's name *)
	    accumulator : bool		(* true if values are cumulative *)
	  }

      (* return the property's name *)
        val nameOf : t -> string

      (* has the property instance been defined? *)
	val isDefined : t -> bool

      (* does the property allow multiple definitions? *)
	val isAccumulator : t -> bool

      (* set the property's value; will extend the value if the property is an accumulator *)
	val setValue : t * string -> unit

      end

  (* return the view's name *)
    val nameOf : t -> string

  (* does the view have the given name *)
    val isView : Atom.atom -> t -> bool

  (* find the property instance for the given view, entity, and property name *)
    val findProp : t * entity * Atom.atom -> Prop.t option

  (* `getValue name (view, entity, default)` gets the value for the property `name`
   * associated with `entity` in the `view`.  It returns `[default]` if there is
   * no value set.
   *)
    val getValue : Atom.atom -> t * entity * string -> string list

  (* a view template describes the properties that the various entities of a module
   * might have.
   *)
    type template = {
	moduleProps : Prop.desc list,
	typeProps : Prop.desc list,
	consProps : Prop.desc list
      }

  (* create a new view with the given name and properties *)
    val new : string * template -> t

  end = struct

    structure ATbl = AtomTable

    datatype entity
      = Module of AST.ModuleId.t
      | Type of AST.TypeId.t
      | Constr of AST.ConId.t

  (* hash table on entities *)
    structure ETbl = HashTableFn (
      struct
	type hash_key = entity
	fun hashVal (Module id) = Word.<<(AST.ModuleId.hash id, 0w2) + 0w1
	  | hashVal (Type id) = Word.<<(AST.TypeId.hash id, 0w2) + 0w2
	  | hashVal (Constr id) = Word.<<(AST.ConId.hash id, 0w2) + 0w3
	fun sameKey (Module id1, Module id2) = AST.ModuleId.same(id1, id2)
	  | sameKey (Type id1, Type id2) = AST.TypeId.same(id1, id2)
	  | sameKey (Constr id1, Constr id2) = AST.ConId.same(id1, id2)
	  | sameKey _ = false
      end)

    datatype prop = Prop of {
	name : Atom.atom,
	entity : entity,
	accumulator : bool,		(* true if values are cumulative *)
	value : string list ref
      }

    structure Prop =
      struct
	type t = prop

	datatype desc = Desc of {
	    name : Atom.atom,		(* the property's name *)
	    accumulator : bool		(* true if values are cumulative *)
	  }

	fun nameOf (Prop{name, ...}) = Atom.toString name

	fun isDefined (Prop{value, ...}) = not(List.null(!value))

	fun isAccumulator (Prop{accumulator, ...}) = accumulator

	fun setValue (Prop{value, accumulator, ...}, v) =
	      if accumulator
		then value := !value @ [v]
		else value := [v]

	fun getValue (Prop{value, ...}) = !value

      (* create a new property instance for an entity *)
	fun new (Desc{name, accumulator}, entity) = Prop{
		name = name,
		entity = entity,
		accumulator = accumulator,
		value = ref[]
	      }
      end

  (* table to map entities to their property tables *)
    type entity_tbl = Prop.t ATbl.hash_table ETbl.hash_table

  (* a view template describes the properties that the various entities of a module
   * might have.
   *)
    type template = {
	moduleProps : Prop.desc list,
	typeProps : Prop.desc list,
	consProps : Prop.desc list
      }

    datatype t = View of {
	name : Atom.atom,	(* the view's name *)
	template : {		(* template that specifies which properties are supported by
				 * the view *)
	    moduleProps : Prop.desc ATbl.hash_table,
	    typeProps : Prop.desc ATbl.hash_table,
	    consProps : Prop.desc ATbl.hash_table
	  },
	eTbl : entity_tbl	(* table to map entities to their property tables *)
      }

  (* create a new view *)
    fun new (name, tmp : template) = let
	  fun mkTbl pds = let
		val tbl = ATbl.mkTable(16, Fail "template table")
		fun insert (pd as Prop.Desc{name, ...}) = ATbl.insert tbl (name, pd)
		in
		  List.app insert pds;
		  tbl
		end
	  in
	    View{
		name = Atom.atom name,
		template = {
		    moduleProps = mkTbl (#moduleProps tmp),
		    typeProps = mkTbl (#typeProps tmp),
		    consProps = mkTbl (#consProps tmp)
		  },
		eTbl = ETbl.mkTable (64, Fail "view-entity table")
	      }
	  end

    fun isView name' (View{name, ...}) = Atom.same(name, name')

    fun nameOf (View{name, ...}) = Atom.toString name

  (* find a property instance; we create a new instance from the template if necessary *)
    fun findProp (View{eTbl, template, ...}, entity, name) = let
	(* extract the appropriate prop descriptor table for the entity *)
	  fun propDescTable () = (case entity
		 of Module _ => #moduleProps template
		  | Type _ => #typeProps template
		  | Constr _ => #consProps template
		(* end case *))
	  in
	    case ETbl.find eTbl entity
	     of SOME pTbl => (case ATbl.find pTbl name
		   of NONE => (case ATbl.find (propDescTable ()) name
			 of SOME pdesc => let
			    (* create the property instance for the entity *)
			      val prop = Prop.new (pdesc, entity)
			      in
				ATbl.insert pTbl (name, prop); (* record property instance *)
				SOME prop
			      end
			  | NONE => NONE (* unknown property *)
			(* end case *))
		    | someProp => someProp
		  (* end case *))
	      | NONE => (case ATbl.find (propDescTable ()) name
		   of SOME pdesc => let
		      (* create the property instance for the entity *)
			val prop = Prop.new (pdesc, entity)
		      (* create new property table for the entity *)
			val pTbl = ATbl.mkTable(16, Fail "prop table")
			in
			  ETbl.insert eTbl (entity, pTbl); (* record property table *)
			  ATbl.insert pTbl (name, prop); (* record property instance *)
			  SOME prop
			end
		    | NONE => NONE
		  (* end case *))
	    (* end case *)
	  end

    fun getValue name (View{eTbl, ...}, entity, default) = (
	  case ETbl.find eTbl entity
	     of SOME pTbl => (case ATbl.find pTbl name
		   of SOME(Prop{value=ref[], ...}) => [default]
		    | SOME(Prop{value=ref s, ...}) => s
		    | NONE => [default]
		  (* end case *))
	      | NONE => [default]
	  (* end case *))

  end
