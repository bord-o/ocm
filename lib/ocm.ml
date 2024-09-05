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

fun bool_and : bool  bool  -> bool
             = True  True  -> True
             | True  False -> False
             | False True  -> False
             | False False -> False

fun modus_ponens: bool  bool  -> bool
             = True  x     -> bool_and True True
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
type env = (id * expr) list [@@deriving show]

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
  | x :: xs ->
      let matches, env = predicate x in
      if matches then Option.bind env (fun e -> Some (e, x))
      else first_match predicate xs

(* tries to match args to a pattern, if successful, return the bindings *)
let rec matches (args : expr list) (cons : constructor list) (env : env) =
  (* List.iter (fun (id,expr) -> Printf.printf "Binding: %s to %s\n" id (show_expr expr) ) env; *)
  match (args, cons) with
  | [], [] -> Some env
  | Val (vid, vargs) :: xs, Binder id :: ys ->
      matches xs ys ((id, Val (vid, vargs)) :: env)
  | Val (arg_id, sub_expr) :: _xs, C (cons_id, sub_cons) :: _ys ->
      if arg_id = cons_id then matches sub_expr sub_cons env else None
  | _ -> None

let perform_match (called : expr list) (patterns : pattern list) : expr * env =
  (* List.iter (fun c -> Printf.printf "Called with Arg : %s\n" (show_expr c)) called; *)
  (* print_endline "Possible Patterns: "; *)
  (* List.iter (fun c -> Printf.printf "    %s\n" (show_pattern c)) patterns; *)
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
  let mo =
    first_match
      (fun (_, cons, _) ->
        let matched = matches called cons [] in
        (Option.is_some matched, matched))
      patterns
  in
  let env, pattern =
    if Option.is_none mo then failwith "Pattern matching failed"
    else Option.get mo
  in

  (* print_endline "Env:"; *)
  (* print_endline (show_env env); *)
  (* print_endline "Chosen Pattern:"; *)
  (* print_endline (show_pattern pattern); *)
  let rhs =
    match pattern with
    | _, _, Expr rhs -> rhs
    | _, _, Hole -> failwith "UNIMPLEMENTED: Holes"
  in
  (rhs, env)

let is_binder = function Binder _ -> true | C (_, _) -> false

let rec subst (rhs : expr) (env : env) : expr =
  match (rhs : expr) with
  | App (e, args) -> App (subst e env, List.map (fun arg -> subst arg env) args)
  | Id name -> (
      match List.assoc_opt name env with
      | Some binding_expr -> binding_expr
      | None -> Id name)
  | Val (cname, args) -> Val (cname, List.map (fun arg -> subst arg env) args)

(*
subst needs to fill the the binders in the target pattern in with values from args
it needs to return this substitued expression

fun length : list -> nat
  = length Nil -> Z
  | length (Cons x xs) -> S (length xs)

length (Cons (S Z) Nil)
length (Cons x xs) -> S (length xs)

We already know that this is the pattern we need
We need to match x -> (S Z) and xs -> Nil, returning S (length Nil)

In a more complicated case:
length (Cons Z (Cons Z (Cons Z Nil)))
length (Cons x (Cons x2 xs)) -> S (length (Cons x2 xs))
We need to match x -> Z and x2 -> Z and xs -> Cons Z Nil

So we go through the 
*)

let rec eval (expr : expr) (env : typdef list * fn list * value list) =
  let _types, funcs, _values = env in
  let inner_eval e = eval e env in
  match expr with
  | App (Id name, call_args) ->
      let _func_name, _func_type, (func_patterns : pattern list) =
        funcs |> lookup_func name |> Option.get
      in
      if
        List.length ((fun (_, x, _) -> x) (List.hd func_patterns))
        <> List.length call_args
      then failwith "Called with wrong arity";
      let cbv_args = List.map (fun arg -> eval arg env) call_args in
      let expr, env = perform_match cbv_args func_patterns in
      let bound = subst expr env in
      inner_eval bound
  | Id _name -> Val ("UNIMPLEMENTED", [])
  | Val (cname, []) -> Val (cname, []) (* this is the terminating case *)
  | Val (cname, args) -> Val (cname, List.map inner_eval args)
  | App (_, _) -> failwith "Tried to apply something thats not an identifier"

let rec long_step e env =
  let next = eval e env in
  if e = next then e else long_step next env

let run (p : prog) =
  let types, funcs, expr = p in
  let res = eval expr (types, funcs, []) in
  print_endline "Result: ";
  print_endline (show_expr res)
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
