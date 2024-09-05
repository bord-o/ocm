(* let () = Printf.printf "%s" (Ocm.show_prog Ocm.example) *)
open Ocm

let fib_example : prog =
  ( [ ("nat", [ ("Z", []); ("S", [ "nat" ]) ]) ],
    [
      ( "add",
        Arr ([ Lit "nat"; Lit "nat" ], Lit "nat"),
        [
          ("add", [ Binder "n"; C ("Z", []) ], Expr (Id "n"));
          ( "add",
            [ Binder "n"; C ("S", [ Binder "m" ]) ],
            Expr (Val ("S", [ App (Id "add", [ Id "n"; Id "m" ]) ])) );
        ] );
      ( "pred",
        Arr ([ Lit "nat" ], Lit "nat"),
        [
          ("pred", [ C ("Z", []) ], Expr (Val ("Z", [])));
          ("pred", [ C ("S", [ Binder "a" ]) ], Expr (Id "a"));
        ] );
      ( "fib",
        Arr ([ Lit "nat" ], Lit "nat"),
        [
          ("fib", [ C ("Z", []) ], Expr (Val ("Z", [])));
          ( "fib",
            [ C ("S", [ C ("Z", []) ]) ],
            Expr (Val ("S", [ Val ("Z", []) ])) );
          ( "fib",
            [ Binder "b" ],
            Expr
              (App
                 ( Id "add",
                   [
                     App(Id "fib", [App (Id "pred", [ Id "b" ])]);
                     App(Id "fib", [App (Id "pred", [ App (Id "pred", [ Id "b" ]) ])]);
                   ] )) );
        ] );
      (*
        fun pred : nat -> nat
          = Z -> Z
          | S n -> n

        fun fib : nat -> nat
          = Z -> Z
          | S Z -> S Z
          | n -> add (pred n) (pred (pred n))

        *)
    ],
    App
      ( Id "fib",
        [Val("S", [Val ("S", [Val("S", [Val ("S", [Val ("S", [Val("S", [Val ("S", [Val("S", [Val("S", [Val ("S", [Val("S", [Val ("S", [Val ("S", [Val("S", [Val ("S", [Val("S", [Val("S", [Val ("S", [Val("S", [Val ("S", [Val ("S", [Val("S", [Val ("S", [Val("Z", [])])])])])])])])])])])])])])])])])])])])])])])])]
        )
)
        
(* let ans = run fib_example *)

let () = Frontend.main ()

