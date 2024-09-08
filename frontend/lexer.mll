(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

{
  open Parser

  exception Error of string
}

rule token = parse
[' ' '\t' '\r'] { token lexbuf }
| '\n'            { Lexing.new_line lexbuf; token lexbuf }
| ';'
    { SEMICOLON }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "type"
    { TYPE }
| "fun"
    { FUN }
| ['A'-'Z']['a'-'z']* as capitalid
    { CONSTRUCTOR capitalid }
| ['a'-'z']+ as lowerid
    { IDENT lowerid }
| "->"
    { ARROW}
| '='
    { EQ}
| '|'
    { BAR }
| ':'
    { COLON }
| ','
    { COMMA }
| '?'
    { QMARK }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" ((Lexing.lexeme_start_p lexbuf).pos_lnum) )) }
