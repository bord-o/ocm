open Lexing

let filename = "/home/bordo/ocm/frontend/test.txt"

let parse_file f =
  let input = In_channel.with_open_text f In_channel.input_all in
  let filebuf = Lexing.from_string input in
  try
    print_endline "Parsed:";
    let ast = Parser.program Lexer.token filebuf in
    print_endline (Ocm.show_prog ast);
    Some ast
  with
  | Lexer.Error msg ->
      Printf.eprintf "%s%!" msg;
      None
  | Parser.Error ->
      Printf.eprintf "At line %d, offset: %d: syntax error.\n%!"
        (Lexing.lexeme_start_p filebuf).pos_lnum
        ((Lexing.lexeme_start_p filebuf).pos_cnum
       - (Lexing.lexeme_start_p filebuf).pos_bol);
      None

let main () =
  let input = In_channel.with_open_text filename In_channel.input_all in
  let filebuf = Lexing.from_string input in
  try print_endline (Ocm.show_prog (Parser.program Lexer.token filebuf)) with
  | Lexer.Error msg -> Printf.eprintf "%s%!" msg
  | Parser.Error ->
      Printf.eprintf "At line %d, offset: %d: syntax error.\n%!"
        (Lexing.lexeme_start_p filebuf).pos_lnum
        ((Lexing.lexeme_start_p filebuf).pos_cnum
       - (Lexing.lexeme_start_p filebuf).pos_bol)
