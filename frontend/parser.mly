%{
	open Ocm

	type funOrType = 
		F of fn
		| T of typdef
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
	tfs=type_or_fun_defs SEMICOLON e=expr EOF {
	(List.filter_map (fun def -> match def with T d -> Some d | _ -> None ) tfs,
	List.filter_map (fun def -> match def with F d -> Some d | _ -> None ) tfs,
	 e)
	}

type_or_fun_def:
	t=typedef {T t}
	| f=fundef {F f}

type_or_fun_defs:
	tf=type_or_fun_def {[tf]}
	| tf=type_or_fun_def tfs=type_or_fun_defs {tf::tfs}

fundef:
	FUN i=IDENT COLON f=funtype EQ ps=patterns {i, f, List.map (fun (clist, rhs) -> (i, clist, rhs)) ps}

funtype:
	ts=typeargs ARROW t2=IDENT {Arr ((List.map (fun i -> Lit i) ts), Lit t2)}

typeargs:
	t=IDENT {[t]}
	| t=IDENT COMMA ts=typeargs {t::ts}

patterns:
	p=pattern {[p]}
	| p=pattern BAR ps=patterns {p::ps}

pattern:
	p=pattern_lhs_list ARROW r=rhs {(p,r)}

rhs:
	e=expr {Expr e}
	| QMARK {Hole}

exprs:
	e=expr {[e]}
	| e=expr es=exprs {e::es} 

expr:
	e=expr_noparen  {e}
	| e=expr_paren {e}

expr_noparen:
	i=idlit {i}
	| v=valu {v}
	| a=app {a}

expr_paren:
	LPAREN e=expr RPAREN {e}

app:
	e=expr es=exprs {App(e, es)}  
	// TODO: This is not an arbitrary expression, if it is a Val it should be in parentheses 
	// This reduction is solved in the desired way automatically by menhir right now so I'm going to leave it

valu:
	c=CONSTRUCTOR {Val(c, [])}
	| c=CONSTRUCTOR es=exprs {Val(c, es)} 

idlit:
	i=IDENT {Id i}

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
	
