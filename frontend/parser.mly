%{
	open Ocm
%}
%token <int> INT
%token SEMICOLON
%token EOF
%token <Ocm.capital_id> CONSTRUCTOR 
%token <Ocm.id> IDENT
%token TYPE EQ BAR ARROW COLON LPAREN RPAREN QMARK FUN

/* changed the type, because the script does not return one value, but all
 * results which are calculated in the file */
%start <prog> program

%%
program:
	t=typedefs EOF {(t, [], Id "test")}

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
	
