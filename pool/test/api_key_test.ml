open Pool_message
module Model = Test_utils.Model
module Command = Cqrs_command.Api_key_command

let get_exn = Test_utils.get_or_failwith
let database_label = Test_utils.Data.database_label

module Data = struct
  let name = "API Key Name"

  let expires_at =
    Ptime.Span.of_int_s @@ (60 * 60 * 2)
    |> Ptime.add_span (Ptime_clock.now ())
    |> CCOption.get_exn_or "Failed to create expires_at"
  ;;

  let urlencoded =
    [ Field.(show Name), [ name ]
    ; Field.(show ExpiresAt), [ Ptime.to_rfc3339 expires_at ]
    ]
  ;;
end

let create () =
  let open Api_key in
  let id = Id.create () in
  let token = Token.generate () in
  let events =
    let open CCResult in
    let open Command.Create in
    Data.urlencoded |> decode >>= handle ~id ~token
  in
  let expected =
    let api_key =
      Api_key.create
        ~id
        ~token
        (Name.of_string Data.name)
        (ExpiresAt.create Data.expires_at)
    in
    Ok [ Created api_key |> Pool_event.api_key ]
  in
  Test_utils.check_result ~msg:"Create API Key" events expected
;;

let update () =
  let open Api_key in
  let api_key = Model.create_api_key () in
  let name = "Updated API Key Name" in
  let events =
    let open CCResult in
    let open Command.Update in
    [ Field.(show Name), [ name ] ] |> decode >>= handle api_key
  in
  let expected =
    Ok
      [ Updated (api_key, { api_key with name = Name.of_string name })
        |> Pool_event.api_key
      ]
  in
  Test_utils.check_result ~msg:"Update API Key" events expected
;;

let get_current _ () =
  let open Api_key in
  let testable_key = Alcotest.(option Test_utils.api_key) in
  let hour = Ptime.Span.of_int_s 3600 in
  let create_api_key token expires_at =
    let api_key =
      Model.create_api_key ~id:(Id.create ()) ~token ~expires_at ()
    in
    let%lwt () = Created api_key |> handle_event database_label in
    Lwt.return api_key
  in
  let valid_token = Token.generate () in
  let expires_at =
    Ptime.add_span (Ptime_clock.now ()) hour
    |> CCOption.get_exn_or "Invalid ptime"
    |> ExpiresAt.create
  in
  let%lwt valid_api_key = create_api_key valid_token expires_at in
  let%lwt res = find_by_token database_label (Token.value valid_token) in
  let () =
    Alcotest.check testable_key "found valid api key" (Some valid_api_key) res
  in
  let expires_at =
    Ptime.sub_span (Ptime_clock.now ()) hour
    |> CCOption.get_exn_or "Invalid ptime"
    |> ExpiresAt.create
  in
  let invalid_token = Token.generate () in
  let%lwt (_ : Api_key.t) = create_api_key invalid_token expires_at in
  let%lwt res = find_by_token database_label (Token.value invalid_token) in
  let () =
    Alcotest.check testable_key "did not find expired api key" None res
  in
  Lwt.return ()
;;
