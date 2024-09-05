open Ocm

let example1 : prog =
  ( [ ("nat", [ ("Z", []); ("S", [ "nat" ]) ]) ],
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
          Val ("S", [ Val ("S", [ Val ("S", [ Val ("Z", []) ]) ]) ]);
          Val ("Z", []);
        ] ) )

let%expect_test "example1_out" =
  Ocm.run example1 |> ignore;
  [%expect
    {|
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
            [
              C ("Cons", [ Binder "x"; C ("Cons", [ Binder "y"; Binder "ys" ]) ]);
            ],
            Expr (Val ("S", [ Val ("S", [ App (Id "length", [ Id "ys" ]) ]) ]))
          );
          ( "length",
            [ C ("Cons", [ Binder "x"; Binder "xs" ]) ],
            Expr (Val ("S", [ App (Id "length", [ Id "xs" ]) ])) );
        ] );
    ],
    App
      ( Id "length",
        [
          Val
            ( "Cons",
              [
                Val ("S", [ Val ("Z", []) ]);
                Val
                  ( "Cons",
                    [
                      Val ("S", [ Val ("S", [ Val ("Z", []) ]) ]);
                      Val ("Nil", []);
                    ] );
              ] );
        ] ) )

let%expect_test "example2_out" =
  Ocm.run example2 |> ignore;
  [%expect
    {|
    Result:
    (Ocm.Val ("S", [(Ocm.Val ("S", [(Ocm.Val ("Z", []))]))]))
    |}]

let example3 : prog =
  ( [ ("nat", [ ("Z", []); ("S", [ "nat" ]) ]) ],
    [
      (*| add n Z -> n
        | add n (S m) -> S (add n m)*)
      (*
        naturals =
          | Zero
          | Successor of a natural
      *)
      (*| mult n Z ->  Z
        | mult n (S Z) ->  n
        | add n (S m) -> add n (mult n m) *)
      ( "add",
        Arr ([ Lit "nat"; Lit "nat" ], Lit "nat"),
        [
          ("add", [ Binder "n"; C ("Z", []) ], Expr (Id "n"));
          ( "add",
            [ Binder "n"; C ("S", [ Binder "m" ]) ],
            Expr (Val ("S", [ App (Id "add", [ Id "n"; Id "m" ]) ])) );
        ] );
      ( "mult",
        Arr ([ Lit "nat"; Lit "nat" ], Lit "nat"),
        [
          ("add", [ Binder "n"; C ("Z", []) ], Expr (Val ("Z", [])));
          ("add", [ Binder "n"; C ("S", [ C ("Z", []) ]) ], Expr (Id "n"));
          ( "add",
            [ Binder "n"; C ("S", [ Binder "m" ]) ],
            Expr
              (App (Id "add", [ Id "n"; App (Id "mult", [ Id "n"; Id "m" ]) ]))
          );
        ] );
    ],
    App
      ( Id "mult",
        [
          Val ("S", [ Val ("S", [ Val ("S", [ Val ("Z", []) ]) ]) ]);
          Val ("S", [ Val ("S", [ Val ("S", [ Val ("Z", []) ]) ]) ]);
        ] ) )

let%expect_test "example3_out" =
  Ocm.run example3 |> ignore;
  [%expect
    {|
    Result:
    (Ocm.Val ("S",
       [(Ocm.Val ("S",
           [(Ocm.Val ("S",
               [(Ocm.Val ("S",
                   [(Ocm.Val ("S",
                       [(Ocm.Val ("S",
                           [(Ocm.Val ("S",
                               [(Ocm.Val ("S",
                                   [(Ocm.Val ("S", [(Ocm.Val ("Z", []))]))]))
                                 ]
                               ))
                             ]
                           ))
                         ]
                       ))
                     ]
                   ))
                 ]
               ))
             ]
           ))
         ]
       ))
    |}]
