%{
	open Ocm
%}
%token <int> INT
%token SEMICOLON
%token EOF
%token <Ocm.capital_id> CONSTRUCTOR 
%token <Ocm.id> IDENT
%token TYPE EQ BAR ARROW COLON LPAREN RPAREN QMARK FUN COMMA

/* 

type expr =
  | App of expr * expr list (* an application *)
  | Id of id (* this is a function defined above *)
  | Val of
      capital_id
      * expr list (* This is an instantiation of a type using a constructor *)
[@@deriving show]

type rhs = Expr of expr | Hole [@@deriving show]
type pattern = id * constructor list * rhs [@@deriving show]
type fn = id * typ * pattern list [@@deriving show]

type constructor = C of capital_id * constructor list | Binder of id


*/
%start <prog> program

%%
program:
	f=fundefs EOF {([], f, Id "test")}

fundefs:
	FUN i=IDENT COLON f=funtype EQ ps=patterns {[i, f, List.map (fun (clist, rhs) -> (i, clist, rhs)) ps]}

funtype:
	t1=IDENT ARROW t2=IDENT {Arr ([Lit t1], Lit t2)}

patterns:
	p=pattern {[p]}
	| p=pattern BAR ps=patterns {p::ps}

pattern:
	p=pattern_lhs_list ARROW r=rhs {(p,r)}

rhs:
	i=IDENT {Expr(Id i)}

pattern_lhs_list:
	p=pattern_lhs {[p]}
	| p=pattern_lhs COMMA ps=pattern_lhs_list {p::ps}

pattern_lhs:
	b=binder {b}
	| c=pattern_constructor {c}

pattern_constructor:
	c=CONSTRUCTOR {C(c, [])}
	| c=CONSTRUCTOR args=cargs {C (c, args)}

cargs:
	c=carg {[c]}
	| c=carg cs=cargs {c::cs}

carg:
	LPAREN pc=pattern_constructor RPAREN {pc}
	| b=binder {b}
	| c=CONSTRUCTOR {C(c, [])}

binder:
	i=IDENT {Binder i}


typedefs:
	t=typedef {[t]}
	| t=typedef ts=typedefs {t::ts}

/* type typdef = id * (capital_id * id list) list [@@deriving show] */
typedef:
	TYPE i=IDENT EQ cons=type_constructors  {i, cons}

type_constructors:
	c=constructor_with_args {[c]}
	| c=constructor_with_args BAR cs=type_constructors {c::cs}

constructor_with_args:
	c=CONSTRUCTOR a=args {(c, a)}
	| c=CONSTRUCTOR {(c, [])}

args:
	i=IDENT {[i]}
	| i=IDENT a=args {i::a}
	
