(* let () = Printf.printf "%s" (Ocm.show_prog Ocm.example) *)
open Ocm


        
(* let ans = run fib_example *)


let ast = Frontend.parse_file "/home/bordo/ocm/bin/test.txt" 

let res  =
  match ast with
  None -> print_endline "failed to parse"
  | Some prog -> Ocm.run prog

