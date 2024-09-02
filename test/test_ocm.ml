open Ocm

let example1 : prog =
  ( [
      ("nat", [ ("Z", []); ("S", [ "nat" ]) ]);
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
    ],
    App
      ( Id "add",
        [ 
          Val ("S", [Val ("S", [Val ("S", [Val ("Z", [])])])]);
          Val ("Z",[])
        ]))

let%expect_test "example1_out"=
  Ocm.run example1 |> ignore;
  [%expect {|
    Called with Arg : (Ocm.Val ("S", [(Ocm.Val ("S", [(Ocm.Val ("S", [(Ocm.Val ("Z", []))]))]))]))
    Called with Arg : (Ocm.Val ("Z", []))
    Possible Patterns:
        ("add", [(Ocm.Binder "n"); (Ocm.C ("Z", []))], (Ocm.Expr (Ocm.Id "n")))
        ("add", [(Ocm.Binder "n"); (Ocm.C ("S", [(Ocm.Binder "m")]))],
     (Ocm.Expr
        (Ocm.Val ("S", [(Ocm.App ((Ocm.Id "add"), [(Ocm.Id "n"); (Ocm.Id "m")]))]
           ))))
    Env:
    [("n",
      (Ocm.Val ("S", [(Ocm.Val ("S", [(Ocm.Val ("S", [(Ocm.Val ("Z", []))]))]))]
         )))
      ]
    Chosen Pattern:
    ("add", [(Ocm.Binder "n"); (Ocm.C ("Z", []))], (Ocm.Expr (Ocm.Id "n")))
    Result:
    (Ocm.Val ("S", [(Ocm.Val ("S", [(Ocm.Val ("S", [(Ocm.Val ("Z", []))]))]))]))
    |}]


let example2 : prog =
  ( [
      ("nat", [ ("Z", []); ("S", [ "nat" ]) ]);
      ("list", [ ("Nil", []); ("Cons", [ "nat"; "list" ]) ]);
    ],
    [
      ( "length",
        Arr ([ Lit "list" ], Lit "nat"),
        [
          ("length", [ C ("Nil", []) ], Expr (Val ("Z", [])));
          ( "length",
            [ C ("Cons", [ Binder "x"; C ("Cons", [Binder "y"; Binder "ys"]) ]) ],
            Expr ( Val( "S", [Val ("S", [ App (Id "length", [ Id "ys" ]) ])]) ) );
          ( "length",
            [ C ("Cons", [ Binder "x"; Binder "xs" ]) ],
            Expr (Val ("S", [ App (Id "length", [ Id "xs" ]) ])) );
        ] );
    ],

    App
      ( Id "length",
        [  Val("Cons", [ Val ("S", [ Val ("Z", []) ]); Val ("Cons", [ Val ("S", [ Val ("S", [Val ("Z", [])]) ]); Val ("Nil", []) ])]) ] ) )

let%expect_test "example2_out"=
  Ocm.run example2 |> ignore;
  [%expect {|
    Called with Arg : (Ocm.Val ("Cons",
       [(Ocm.Val ("S", [(Ocm.Val ("Z", []))]));
         (Ocm.Val ("Cons",
            [(Ocm.Val ("S", [(Ocm.Val ("S", [(Ocm.Val ("Z", []))]))]));
              (Ocm.Val ("Nil", []))]
            ))
         ]
       ))
    Possible Patterns:
        ("length", [(Ocm.C ("Nil", []))], (Ocm.Expr (Ocm.Val ("Z", []))))
        ("length",
     [(Ocm.C ("Cons",
         [(Ocm.Binder "x");
           (Ocm.C ("Cons", [(Ocm.Binder "y"); (Ocm.Binder "ys")]))]
         ))
       ],
     (Ocm.Expr
        (Ocm.Val ("S",
           [(Ocm.Val ("S", [(Ocm.App ((Ocm.Id "length"), [(Ocm.Id "ys")]))]))]))))
        ("length", [(Ocm.C ("Cons", [(Ocm.Binder "x"); (Ocm.Binder "xs")]))],
     (Ocm.Expr (Ocm.Val ("S", [(Ocm.App ((Ocm.Id "length"), [(Ocm.Id "xs")]))]))))
    Env:
    [("ys", (Ocm.Val ("Nil", [])));
      ("y", (Ocm.Val ("S", [(Ocm.Val ("S", [(Ocm.Val ("Z", []))]))])));
      ("x", (Ocm.Val ("S", [(Ocm.Val ("Z", []))])))]
    Chosen Pattern:
    ("length",
     [(Ocm.C ("Cons",
         [(Ocm.Binder "x");
           (Ocm.C ("Cons", [(Ocm.Binder "y"); (Ocm.Binder "ys")]))]
         ))
       ],
     (Ocm.Expr
        (Ocm.Val ("S",
           [(Ocm.Val ("S", [(Ocm.App ((Ocm.Id "length"), [(Ocm.Id "ys")]))]))]))))
    Called with Arg : (Ocm.Val ("Nil", []))
    Possible Patterns:
        ("length", [(Ocm.C ("Nil", []))], (Ocm.Expr (Ocm.Val ("Z", []))))
        ("length",
     [(Ocm.C ("Cons",
         [(Ocm.Binder "x");
           (Ocm.C ("Cons", [(Ocm.Binder "y"); (Ocm.Binder "ys")]))]
         ))
       ],
     (Ocm.Expr
        (Ocm.Val ("S",
           [(Ocm.Val ("S", [(Ocm.App ((Ocm.Id "length"), [(Ocm.Id "ys")]))]))]))))
        ("length", [(Ocm.C ("Cons", [(Ocm.Binder "x"); (Ocm.Binder "xs")]))],
     (Ocm.Expr (Ocm.Val ("S", [(Ocm.App ((Ocm.Id "length"), [(Ocm.Id "xs")]))]))))
    Env:
    []
    Chosen Pattern:
    ("length", [(Ocm.C ("Nil", []))], (Ocm.Expr (Ocm.Val ("Z", []))))
    Result:
    (Ocm.Val ("S", [(Ocm.Val ("S", [(Ocm.Val ("Z", []))]))]))
    |}]
