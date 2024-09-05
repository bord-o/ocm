/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Rocquencourt                                  */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2008 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the Q Public License version 1.0, with the change  */
/*  described in file LICENSE.                                            */
/*                                                                        */
/**************************************************************************/

%token <int> INT
%token SEMICOLON
%token EOF
%token <string> CONSTRUCTOR 
%token <string> IDENT
%token TYPE EQ BAR ARROW COLON LPAREN RPAREN QMARK

/* changed the type, because the script does not return one value, but all
 * results which are calculated in the file */
%start <int> main

%%

/* TODO: write the grammar */
/* the calculated results are accumalted in an OCaml int list */
main:
| stmt = statement EOF { stmt }

/* expressions end with a semicolon, not with a newline character */
statement:
| e = expr SEMICOLON { e }

expr:
| i = INT
    { i }
