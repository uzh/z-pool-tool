open Pool_message
module Language = Pool_common.Language

let current_user () = Integration_utils.AdminRepo.create () |> Lwt.map Pool_context.admin
let database_label = Test_utils.Data.database_label

let save_custom_fields current_user custom_field contact =
  let public =
    Custom_field_test.Data.to_public Contact.(contact |> id |> Id.to_common) custom_field
  in
  let events =
    [ Custom_field.Created custom_field |> Pool_event.custom_field
    ; Custom_field.Published custom_field |> Pool_event.custom_field
    ; Custom_field.AnswerUpserted
        (public, Contact.id contact, Pool_context.Contact contact)
      |> Pool_event.custom_field
    ]
  in
  Pool_event.handle_events database_label current_user events
;;

let update_with_old_version _ () =
  let%lwt () =
    let open CCResult in
    let contact = Test_utils.Model.create_contact () in
    let language = Language.De in
    let contact =
      Contact.{ contact with language_version = 1 |> Pool_common.Version.of_int }
    in
    let field = Field.Language in
    let%lwt partial_update =
      let version = 0 |> Pool_common.Version.of_int in
      Custom_field.validate_partial_update
        contact
        None
        (field, version, [ Pool_common.Language.show language ])
    in
    let expected = Error (Error.MeantimeUpdate field) in
    Alcotest.(
      check
        (result Test_utils.partial_update Test_utils.error)
        "succeeds"
        expected
        partial_update)
    |> Lwt.return
  in
  Lwt.return_unit
;;

let update_custom_field _ () =
  let%lwt () =
    let open CCResult in
    let open Custom_field in
    let%lwt current_user = current_user () in
    let contact = Test_utils.Model.create_contact () in
    let custom_field = Custom_field_test.Data.custom_text_field () in
    let public =
      Custom_field_test.Data.to_public Contact.(id contact |> Id.to_common) custom_field
    in
    let%lwt () = save_custom_fields current_user custom_field contact in
    let language = Pool_common.Language.En in
    let field = Public.to_common_field language public in
    let new_value = "new value" in
    let%lwt partial_update =
      let version = 0 |> Pool_common.Version.of_int in
      Custom_field.validate_partial_update
        contact
        (Some public)
        (field, version, [ new_value ])
    in
    let expected =
      let[@warning "-4"] expected_field =
        match public with
        | Public.Text (p, answer) ->
          let answer =
            answer |> CCOption.map (fun a -> Answer.{ a with value = Some new_value })
          in
          Public.Text (p, answer)
        | _ -> failwith "Wrong field type"
      in
      Ok Custom_field.PartialUpdate.(Custom expected_field |> increment_version)
    in
    Alcotest.(
      check
        (result Test_utils.partial_update Test_utils.error)
        "succeeds"
        expected
        partial_update)
    |> Lwt.return
  in
  Lwt.return_unit
;;

let partial_update_exec
      ?is_admin
      ?(custom_field = Custom_field_test.Data.custom_text_field ())
      ?(value = "testvalue")
      expected
      ()
  =
  let%lwt () =
    let open Custom_field in
    let%lwt current_user = current_user () in
    let contact = Test_utils.Model.create_contact () in
    let public =
      Custom_field_test.Data.to_public Contact.(id contact |> Id.to_common) custom_field
    in
    let%lwt () = save_custom_fields current_user custom_field contact in
    let language = Pool_common.Language.En in
    let field = Public.to_common_field language public in
    let%lwt partial_update =
      let version = 0 |> Pool_common.Version.of_int in
      Custom_field.validate_partial_update
        ?is_admin
        contact
        (Some public)
        (field, version, [ value ])
    in
    Alcotest.(
      check
        (result Test_utils.partial_update Test_utils.error)
        "succeeds"
        expected
        partial_update)
    |> Lwt.return
  in
  Lwt.return_unit
;;

let update_custom_field_with_invalid_answer _ () =
  let validation = [ "text_length_max", "10" ] in
  let custom_field = Custom_field_test.Data.custom_text_field ~validation () in
  let value = "this value is longer than 10" in
  let expected = Error (Error.TextLengthMax 10) in
  partial_update_exec ~custom_field ~value expected ()
;;

let update_admin_input_only_field_as_user _ () =
  let open Custom_field in
  let custom_field =
    Custom_field_test.Data.custom_text_field
      ~admin_input_only:(AdminInputOnly.create true)
      ()
  in
  let expected = Error Error.NotEligible in
  partial_update_exec ~is_admin:false ~custom_field expected ()
;;

let update_non_override_field_as_admin _ () =
  let open Custom_field in
  let custom_field =
    Custom_field_test.Data.custom_text_field
      ~admin_override:(AdminOverride.create false)
      ()
  in
  let expected = Error Error.NotEligible in
  partial_update_exec ~is_admin:true ~custom_field expected ()
;;

let set_value_of_none_required_field_to_null _ () =
  let open Custom_field in
  let custom_field = Custom_field_test.Data.custom_text_field () in
  let value = "" in
  let[@warning "-4"] expected =
    let public =
      Custom_field_test.Data.to_public (Pool_common.Id.create ()) custom_field
    in
    match public with
    | Public.Text (public, _) ->
      Custom_field.PartialUpdate.(
        Custom (Public.Text (public, None)) |> increment_version)
      |> CCResult.return
    | _ -> failwith "Invailid field type "
  in
  partial_update_exec ~custom_field ~value expected ()
;;

let set_value_of_required_field_to_null _ () =
  let required = Custom_field.Required.create true in
  let custom_field = Custom_field_test.Data.custom_text_field ~required () in
  let value = "" in
  let expected = Error Error.NoValue in
  partial_update_exec ~custom_field ~value expected ()
;;

let boolean_custom_field ?(is_required = true) ?(is_admin_input_only = false) field_name =
  let open Custom_field in
  Custom_field_utils.create_custom_field field_name (fun field ->
    Boolean
      { field with
        required = Required.create is_required
      ; admin_input_only = AdminInputOnly.create is_admin_input_only
      })
;;

let boolean_answer contact_id field value =
  let open Custom_field in
  Public.Boolean
    ( { Public.id = id field
      ; name = name field
      ; hint = hint field
      ; validation = Validation.pure
      ; required = required field
      ; admin_override = admin_override field
      ; admin_input_only = admin_input_only field
      ; prompt_on_registration = prompt_on_registration field
      ; version = Pool_common.Version.of_int 0
      }
    , Some (Answer.create (Contact.Id.to_common contact_id) (Some value)) )
;;

(* Any valid answer, so that only the field under test stays unanswered *)
let some_answer entity_uuid =
  let open Custom_field in
  let answer value = Some (Answer.create entity_uuid value) in
  function
  | Public.Boolean (public, _) -> Public.Boolean (public, answer (Some true))
  | Public.Date (public, _) -> Public.Date (public, answer (Some (1990, 1, 1)))
  | Public.Number (public, _) -> Public.Number (public, answer (Some 1))
  | Public.Text (public, _) -> Public.Text (public, answer (Some "answer"))
  | Public.Select (public, options, _) ->
    Public.Select (public, options, answer (CCList.head_opt options))
  | Public.MultiSelect (public, options, _) ->
    let value = CCList.head_opt options |> CCOption.map CCList.return in
    Public.MultiSelect (public, options, answer value)
;;

(* A boolean without an answer is an open question, an answer of 'false' is not *)
let unanswered_boolean_is_an_open_question _ () =
  let open Custom_field in
  let%lwt current_user = current_user () in
  let%lwt contact = Integration_utils.ContactRepo.create () in
  let contact_id = Contact.id contact in
  let entity_uuid = Contact.Id.to_common contact_id in
  let user = Pool_context.Contact contact in
  let field = boolean_custom_field "Owns a bicycle" in
  let admin_only_field = boolean_custom_field ~is_admin_input_only:true "Admin only" in
  let handle_events = Pool_event.handle_events database_label current_user in
  let upsert answers =
    answers
    |> CCList.map (fun answer ->
      AnswerUpserted (answer, contact_id, user) |> Pool_event.custom_field)
    |> handle_events
  in
  let%lwt () =
    [ field; admin_only_field ]
    |> CCList.flat_map (fun field ->
      Pool_event.[ custom_field (Created field); custom_field (Published field) ])
    |> handle_events
  in
  let open_questions () =
    find_unanswered_ungrouped_required_by_contact database_label user contact_id
  in
  let contains custom_field lst =
    lst
    |> CCList.map (fun m -> Public.id m |> Id.value)
    |> CCList.mem ~eq:CCString.equal (id custom_field |> Id.value)
  in
  let%lwt unanswered = open_questions () in
  let () =
    Alcotest.(check bool)
      "unanswered boolean is an open question"
      true
      (contains field unanswered)
  in
  let () =
    Alcotest.(check bool)
      "admin input only field is not an open question"
      false
      (contains admin_only_field unanswered)
  in
  (* Answer everything but the boolean, so that it is the only open question left *)
  let%lwt () =
    unanswered
    |> CCList.filter (fun m -> Id.equal (Public.id m) (id field) |> not)
    |> CCList.map (some_answer entity_uuid)
    |> upsert
  in
  let%lwt all_answered = all_required_answered database_label contact_id in
  let () =
    Alcotest.(check bool) "the unanswered boolean is still an open question" false
    @@ all_answered
  in
  (* 'false' is an answer, not a missing answer *)
  let%lwt () = upsert [ boolean_answer contact_id field false ] in
  let%lwt unanswered = open_questions () in
  let () =
    Alcotest.(check bool)
      "an answer of 'false' closes the question"
      false
      (contains field unanswered)
  in
  let%lwt all_answered = all_required_answered database_label contact_id in
  Alcotest.(check bool) "no open questions are left" true all_answered |> Lwt.return
;;
