Printexc.record_backtrace true

let canary_test () = Alcotest.(check int) "impossible" 4 (2 + 2)

let () =
  let open Alcotest in
  run
    "cqrs commands"
    [ "canary", [ test_case "test suite works" `Quick canary_test ]
    ; ( "participant"
      , [ test_case
            "sign up not allowed suffix"
            `Quick
            Participant_test.sign_up_not_allowed_suffix
        ] )
    ]
;;
