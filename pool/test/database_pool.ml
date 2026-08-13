(* Pool cache tests. Kept out of [integration] on purpose: these point a pool at
   a socket of their own and need no reachable database. *)
let () =
  Lwt_main.run
    (Alcotest_lwt.run
       "database pool"
       Alcotest_lwt.
         [ ( "pool cache"
           , [ test_case
                 "unreachable database settles every query"
                 `Slow
                 Database_pool_test.unreachable_database_settles_every_query
             ] )
         ])
;;
