open Lexing

let filename = "/home/bordo/ocm/frontend/test.txt"

  
let main () =
  let input = In_channel.with_open_text filename In_channel.input_all in
  let filebuf = Lexing.from_string input in
  try
    print_int (Parser.main Lexer.token filebuf)
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start filebuf)



