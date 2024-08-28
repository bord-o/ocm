(*
Ocm

a program is 0+ typedefs and 1+ recursive function definitions

type nat 
  = Z
  | S nat

type list
  = Nil
  | Cons nat list

[] = Nil
[1, 2, 3]
  Cons (S Z) (Cons (S (S Z)) (Cons (S (S (S Z))) (Nil)))

type bool
  = True
  | False

fun bool_and : bool  bool -> bool
             = True  True  -> True
             | True  False -> False
             | False True  -> False
             | False False -> False



fun add : natural -> natural -> natural
  | add n Zero -> n
  | add n (Successor m) -> Successor (add n m)

fun length : list -> nat
  = length Nil -> Z
  | length (Cons x xs) -> S (length xs)

length (Cons 1 Nil)
*)
type id = string [@@deriving show]
type capital_id = string [@@deriving show]

type constructor = C of capital_id * constructor list | Binder of id
[@@deriving show]

type typ =
  | Lit of id
  | Arr of typ list * typ (* no first class functions or currying *)
[@@deriving show]

type typdef = id * (capital_id * id list) list [@@deriving show]

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
type prog = typdef list * fn list * expr [@@deriving show]

let example : prog =
  ( [
      ("nat", [ ("Z", []); ("S", [ "nat" ]) ]);
      ("list", [ ("Nil", []); ("Cons", [ "nat"; "list" ]) ]);
    ],
    [
      (*| add n Z -> n
        | add n (S m) -> S (add n m)*)
      ( "add",
        Arr ([ Lit "nat"; Lit "nat" ], Lit "nat"),
        [
          ("add", [ Binder "n"; C ("Z", []) ], Expr (Id "n"));
          ( "add",
            [ Binder "n"; C ("S", [ Binder "m" ]) ],
            Expr (Val ("S", [ App (Id "add", [ Id "n"; Id "m" ]) ])) );
        ] );
      ( "length",
        Arr ([ Lit "list" ], Lit "nat"),
        [
          ("length", [ C ("Nil", []) ], Expr (Val ("Z", [])));
          ( "length",
            [ C ("Cons", [ Binder "x"; Binder "xs" ]) ],
            Expr (Val ("S", [ App (Id "length", [ Id "xs" ]) ])) );
        ] );
    ],
    App
      ( Id "add",
        [ 
          Val ("Z",[]);
          Val ("S", [Val ("S", [Val ("Z", [])])])
        ]))
    (* App *)
      (* ( Id "length", *)
        (* [ Val ("Nil",[])])) *)
    (* App *)
      (* ( Id "length", *)
        (* [ Val ("Cons", [ Val ("S", [ Val ("Z", []) ]); Val ("Nil", []) ]) ] ) ) *)

let rec lookup_func s = function
  | [] -> None
  | (n, t, p) :: xs -> if n = s then Some (n, t, p) else lookup_func s xs

let rec lookup_type s = function
  | [] -> None
  | (n, t) :: xs -> if n = s then Some (n, t) else lookup_type s xs

let rec lookup_value s = function
  | [] -> None
  | (n, v) :: xs -> if n = s then Some (n, v) else lookup_value s xs

type value = id * expr

let rec first_match predicate = function
  | [] -> None
  | x :: xs -> if predicate x then Some x else first_match predicate xs

let rec matches (args : expr list) (cons : constructor list) =
  match (args, cons) with
  | [], [] -> true
  | Val (_, _) :: xs, Binder id :: ys -> matches xs ys
  | Val (arg_id, sub_expr) :: xs, C (cons_id, sub_cons) :: ys ->
      if arg_id = cons_id then matches sub_expr sub_cons else false
  | _ -> false

let perform_match (called : expr list) (patterns : pattern list) : expr =
  List.iter (fun c -> Printf.printf "Called : %s\n" (show_expr c)) called;

  List.iter (fun c -> Printf.printf "Patterns: %s\n" (show_pattern c)) patterns;
  (*
     in the case of length, we need to match the called arg with a pattern
     this is done by going through the list of patterns in order and finding
     the first pattern that matches all args

     In this context, matching means that the outer most constructor has the same
     shape, and the arguments to the constructor match as well

     Anything matches with an identifier in the constructor

     fun length : list -> nat
       = length Nil -> Z
       | length (Cons x xs) -> S (length xs)

     length (Cons (S Z) Nil)

     if the target constructor doesn't have any bindings we just output the matched expression
     if it does have a binding, we need to add them to the environment with the appropriate values from
     the initial call

  *)
  let mo = first_match (fun (_, cons, _) -> matches called cons) patterns in
  let m =
    if Option.is_none mo then failwith "Pattern matching failed"
    else Option.get mo
  in
  print_endline (show_pattern m);


  Val ("Z", [])

let rec eval (expr : expr) (env : typdef list * fn list * value list) =
  let types, funcs, values = env in
  match expr with
  | App (Id name, call_args) ->
      let func_name, func_type, (func_patterns : pattern list) =
        funcs |> lookup_func name |> Option.get
      in
      if
        List.length ((fun (_, x, _) -> x) (List.hd func_patterns))
        <> List.length call_args
      then failwith "Called with wrong arity";
      let cbv_args = List.map (fun arg -> eval arg env) call_args in
      let target_pattern = perform_match cbv_args func_patterns in
      Val ("Z", [])
  | Id name -> Val ("UNIMPLEMENTED", [])
  | Val (cname, args) -> Val (cname, args)
  | App (_, _) -> failwith "Tried to apply something thats not an identifier"

let run (p : prog) =
  let types, funcs, expr = p in
  eval expr (types, funcs, [])
(*
EVALUATION

App -> 
  look up id,
  look up the arg if not a Val
  find the pattern that the arg matches (Look for outermost constructor)
  follow that pattern match and recursively evaluate that arm with any binders added to the env

Id -> 
  look up the id
  evaluates to the expression stored in the env

Val ->
  this is a constructor of some kind
  just make sure that the args are correct 






*)
