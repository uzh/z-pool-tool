module ContactRepo = Integration_utils.ContactRepo
module Command = Cqrs_command.Duplicate_contacts_command
open Utils.Lwt_result.Infix

let pool = Test_utils.Data.database_label
let get_exn = Test_utils.get_or_failwith
let testable_score = Alcotest.(testable CCFloat.pp CCFloat.equal)

let check_similarity _ () =
  let open Utils.Lwt_result.Infix in
  let open Duplicate_contacts in
  let open Pool_user in
  let check ~result ~expected msg =
    Alcotest.check (Alcotest.option testable_score) msg result expected
  in
  let create_contact ~firstname ~lastname =
    ContactRepo.create
      ~firstname:(Firstname.of_string firstname)
      ~lastname:(Lastname.of_string lastname)
      ()
  in
  let find_duplicate ~target ~comparison =
    let open Contact in
    let%lwt () = Service.run pool (id target |> Id.to_common) in
    find_by_contact pool target
    ||> fst
    ||> CCList.find_opt (fun { contact_a; contact_b; _ } ->
      Id.equal (id contact_a) (id comparison) || Id.equal (id contact_b) (id comparison))
  in
  let%lwt contact_1 = create_contact ~firstname:"John" ~lastname:"Doe" in
  let%lwt contact_2 = create_contact ~firstname:"John" ~lastname:"Doe" in
  let%lwt duplicate_score =
    find_duplicate ~target:contact_1 ~comparison:contact_2
    ||> CCOption.map (fun { score; _ } -> score)
  in
  check ~result:duplicate_score ~expected:(Some 1.0) "found duplicate with score 1";
  let%lwt contact_3 = create_contact ~firstname:"Jane" ~lastname:"Doe" in
  let%lwt duplicate_score =
    find_duplicate ~target:contact_1 ~comparison:contact_3
    ||> CCOption.map (fun { score; _ } -> score)
  in
  check
    ~result:duplicate_score
    ~expected:None
    "do not find duplicate with different given name";
  Lwt.return ()
;;

let merge_contact_fields_command () =
  let open Command.Merge in
  let open Test_utils.Model in
  let open Duplicate_contacts in
  let open Pool_message.Field in
  let open Filter_test.CustomFieldData in
  let create_duplicate ~contact_a ~contact_b =
    { id = Id.create ()
    ; contact_a
    ; contact_b
    ; score = 1.0
    ; ignored = Ignored.create false
    }
  in
  let create_contact ~firstname ~lastname =
    create_contact
      ~firstname:(Pool_user.Firstname.of_string firstname)
      ~lastname:(Pool_user.Lastname.of_string lastname)
      ()
  in
  let common_contact_id contact =
    let open Contact in
    id contact |> Id.to_common
  in
  let contact_id contact = contact |> common_contact_id |> Pool_common.Id.value in
  let field_id field =
    let open Custom_field in
    Public.id field |> Id.to_common |> Pool_common.Id.value
  in
  let nr_siblings contact =
    NrOfSiblings.public ~entity_uuid:(common_contact_id contact) false
  in
  let contact_a = create_contact ~firstname:"John" ~lastname:"Doe" in
  let contact_b = create_contact ~firstname:"Foo" ~lastname:"Bar" in
  let duplicate = create_duplicate ~contact_a ~contact_b in
  let siblings_a = nr_siblings contact_a (Some 1) in
  let siblings_b = nr_siblings contact_b (Some 2) in
  let check_result urlencoded expected =
    let res =
      handle urlencoded duplicate [ NrOfSiblings.field ] ([ siblings_a ], [ siblings_b ])
    in
    let open Test_utils in
    Alcotest.(
      check
        (result merge_contact error)
        "Merge contact with data of contact_a"
        res
        expected)
  in
  (* Select all data of contact_a *)
  let urlencoded =
    let email = [ show EmailAddress, [ contact_id contact_a ] ] in
    let hardcoded =
      read_hardcoded
      |> CCList.map (fun (field, _) -> show field, [ contact_id contact_a ])
    in
    let custom_fields = [ field_id siblings_a, [ contact_id contact_a ] ] in
    email @ hardcoded @ custom_fields
  in
  let expected =
    Ok { contact = contact_a; merged_contact = contact_b; custom_fields = [ siblings_a ] }
  in
  check_result urlencoded expected;
  (* Select all data, except email, of contact_a *)
  let urlencoded =
    let email = [ show EmailAddress, [ contact_id contact_b ] ] in
    let hardcoded =
      read_hardcoded
      |> CCList.map (fun (field, _) -> show field, [ contact_id contact_a ])
    in
    let custom_fields = [ field_id siblings_a, [ contact_id contact_a ] ] in
    email @ hardcoded @ custom_fields
  in
  let expected =
    let open Contact in
    let user =
      Pool_user.
        { (user contact_b) with
          firstname = contact_a |> user |> firstname
        ; lastname = contact_a |> user |> lastname
        }
    in
    let contact =
      { contact_b with
        user
      ; cell_phone = contact_a.cell_phone
      ; language = contact_a.language
      }
    in
    Ok { contact; merged_contact = contact_a; custom_fields = [ siblings_a ] }
  in
  check_result urlencoded expected;
  (* Select custom field of contact_b *)
  let urlencoded =
    let email = [ show EmailAddress, [ contact_id contact_a ] ] in
    let hardcoded =
      read_hardcoded
      |> CCList.map (fun (field, _) -> show field, [ contact_id contact_a ])
    in
    let custom_fields = [ field_id siblings_a, [ contact_id contact_b ] ] in
    email @ hardcoded @ custom_fields
  in
  let expected =
    Ok { contact = contact_a; merged_contact = contact_b; custom_fields = [ siblings_b ] }
  in
  check_result urlencoded expected
;;

module MergeData = struct
  module Testable = struct
    (* TODO: Create global testable module *)
    let email_address = Pool_user.EmailAddress.(Alcotest.testable pp equal)
    let firstname = Pool_user.Firstname.(Alcotest.testable pp equal)
    let lastname = Pool_user.Lastname.(Alcotest.testable pp equal)

    let testable_answer (a, b) test =
      let open Custom_field.Answer in
      let open Alcotest in
      let open CCOption.Infix in
      check (option test) "equal value" (a >>= value) (b >>= value);
      check (option test) "equal admin_value" (a >>= admin_value) (b >>= admin_value)
    ;;
  end

  let test_answer_value (a, b) =
    let open Custom_field in
    let open Public in
    let open Alcotest in
    let select_option = SelectOption.Public.(testable pp equal) in
    let run_test = Testable.testable_answer in
    match[@warning "-4"] a, b with
    | Public.Boolean (_, a), Public.Boolean (_, b) -> run_test (a, b) bool
    | Date (_, a), Date (_, b) -> run_test (a, b) Test_utils.date
    | MultiSelect (_, _, a), MultiSelect (_, _, b) -> run_test (a, b) (list select_option)
    | Number (_, a), Number (_, b) -> run_test (a, b) int
    | Select (_, _, a), Select (_, _, b) -> run_test (a, b) select_option
    | Text (_, a), Text (_, b) -> run_test (a, b) string
    | _, _ -> failwith "Custom field answer types do not match"
  ;;

  let make_contact ~firstname ~lastname =
    ContactRepo.create
      ~firstname:(Pool_user.Firstname.of_string firstname)
      ~lastname:(Pool_user.Lastname.of_string lastname)
      ()
  ;;

  let setup_merge_contacts () =
    let open Integration_utils in
    let open Pool_user in
    let pool = Test_utils.Data.database_label in
    let%lwt current_user = Integration_utils.create_contact_user () in
    let make_field name = CustomFieldRepo.create name (fun a -> Custom_field.Text a) in
    let%lwt field_1 = make_field "F1" in
    let%lwt field_2 = make_field "F2" in
    let%lwt field_3 = make_field "F3" in
    let%lwt contact_a = make_contact ~firstname:"John" ~lastname:"Doe" in
    let%lwt contact_b = make_contact ~firstname:"Jane" ~lastname:"Doe" in
    let duplicate =
      Duplicate_contacts.
        { id = Id.create ()
        ; contact_a
        ; contact_b
        ; score = 1.0
        ; ignored = Ignored.create false
        }
    in
    let save_answer field i contact =
      let open Custom_field in
      let contact_id = Contact.id contact in
      let answer_string =
        Format.asprintf "%s-%i" (Contact.firstname contact |> Firstname.value) i
      in
      let%lwt public =
        find_by_contact pool (Contact.id contact) (id field)
        ||> get_exn
        ||> function[@warning "-4"]
        | Public.Text (field, _) ->
          let answer =
            Answer.create (Contact.Id.to_common contact_id) (Some answer_string)
          in
          Public.Text (field, Some answer)
        | _ -> failwith "Invalid custom field"
      in
      Custom_field.AnswerUpserted (public, contact_id, current_user)
      |> Pool_event.custom_field
      |> Pool_event.handle_event pool current_user
    in
    let%lwt () =
      [ field_1; field_2; field_3 ]
      |> Lwt_list.iteri_s (fun i field ->
        [ contact_a; contact_b ] |> Lwt_list.iter_s (save_answer field i))
    in
    Lwt.return (current_user, duplicate, [ field_1; field_2; field_3 ])
  ;;

  let fields_by_contact current_user fields contact =
    let open Custom_field in
    find_all_by_contact_flat pool current_user (Contact.id contact)
    ||> CCList.filter (fun field ->
      fields |> CCList.exists (fun f -> Id.equal (Public.id field) (id f)))
  ;;

  let make_urlencoded ~email ~firstname ~lastname ~cell_phone ~language custom_fields =
    let id contact = Contact.id contact |> Contact.Id.value in
    let open Pool_message in
    let custom_fields =
      custom_fields
      |> CCList.map (fun (field, contact) ->
        Custom_field.(id field |> Id.value), id contact)
    in
    [ Field.(show EmailAddress), id email
    ; Field.(show Firstname), id firstname
    ; Field.(show Lastname), id lastname
    ; Field.(show CellPhone), id cell_phone
    ; Field.(show Language), id language
    ]
    @ custom_fields
    |> CCList.map (CCPair.map_snd CCList.return)
  ;;

  module T = Testable

  let compare contact ~email ~firstname ~lastname ~cell_phone ~language public_fields =
    let%lwt contact = Contact.find pool (Contact.id contact) ||> get_exn in
    let open Alcotest in
    let run_check testable msg decode expected =
      check testable msg (decode contact) (decode expected)
    in
    let compare_custom_field_value public =
      let open Custom_field in
      let%lwt stored =
        find_by_contact pool (Contact.id contact) (Public.id public) ||> get_exn
      in
      test_answer_value (stored, public);
      Lwt.return ()
    in
    run_check T.email_address "equal email" Contact.email_address email;
    run_check T.firstname "equal firstname" Contact.firstname firstname;
    run_check T.lastname "equal lastname" Contact.lastname lastname;
    run_check (option Test_utils.phone_nr) "equal phone" Contact.cell_phone cell_phone;
    run_check
      (option Test_utils.language)
      "equal lang"
      (fun c -> c.Contact.language)
      language;
    let%lwt () = public_fields |> Lwt_list.iter_s compare_custom_field_value in
    Lwt.return ()
  ;;
end

let override_a_with_b _ () =
  let open Duplicate_contacts in
  let open MergeData in
  let%lwt current_user, duplicate, fields = setup_merge_contacts () in
  let { contact_a; contact_b; _ } = duplicate in
  let fields_by_contact = fields_by_contact current_user fields in
  let%lwt fields_a = fields_by_contact contact_a in
  let%lwt fields_b = fields_by_contact contact_b in
  let urlencoded =
    fields
    |> CCList.map (fun field -> field, contact_b)
    |> make_urlencoded
         ~email:contact_a
         ~firstname:contact_b
         ~lastname:contact_b
         ~cell_phone:contact_b
         ~language:contact_b
  in
  let%lwt () =
    Command.Merge.handle urlencoded duplicate fields (fields_a, fields_b)
    |> Lwt_result.lift
    >>= merge pool
    ||> get_exn
  in
  let%lwt () =
    compare
      contact_a
      ~email:contact_a
      ~firstname:contact_b
      ~lastname:contact_b
      ~cell_phone:contact_b
      ~language:contact_b
      fields_b
  in
  Lwt.return_unit
;;

let override_b_with_a _ () =
  let open Duplicate_contacts in
  let open MergeData in
  let%lwt current_user, duplicate, fields = setup_merge_contacts () in
  let { contact_a; contact_b; _ } = duplicate in
  let fields_by_contact = fields_by_contact current_user fields in
  let%lwt fields_a = fields_by_contact contact_a in
  let%lwt fields_b = fields_by_contact contact_b in
  let urlencoded =
    fields
    |> CCList.map (fun field -> field, contact_a)
    |> make_urlencoded
         ~email:contact_b
         ~firstname:contact_a
         ~lastname:contact_a
         ~cell_phone:contact_a
         ~language:contact_a
  in
  let%lwt () =
    Command.Merge.handle urlencoded duplicate fields (fields_a, fields_b)
    |> Lwt_result.lift
    >>= merge pool
    ||> get_exn
  in
  let%lwt () =
    compare
      contact_b
      ~email:contact_b
      ~firstname:contact_a
      ~lastname:contact_a
      ~cell_phone:contact_a
      ~language:contact_a
      fields_a
  in
  Lwt.return_unit
;;

let override_with_participations _ () =
  let open Duplicate_contacts in
  let open MergeData in
  let%lwt current_user = Integration_utils.create_contact_user () in
  let%lwt contact_a = make_contact ~firstname:"John11" ~lastname:"Doe11" in
  let%lwt contact_b = make_contact ~firstname:"Jane22" ~lastname:"Doe22" in
  let%lwt experiment = Integration_utils.ExperimentRepo.create () in
  let%lwt session = Integration_utils.SessionRepo.create experiment () in
  let assignment = Assignment.create contact_a in
  (* Store participation of contact 1 *)
  let%lwt () =
    let open Assignment in
    let handle = Pool_event.handle_events pool current_user in
    let%lwt () =
      [ Invitation.(Created { experiment; mailing = None; contacts = [ contact_a ] })
        |> Pool_event.invitation
      ; Contact.(
          Updated { contact_a with num_invitations = NumberOfInvitations.of_int 1 }
          |> Pool_event.contact)
      ; Created (assignment, session.Session.id) |> Pool_event.assignment
      ; Updated
          ( assignment
          , { assignment with
              participated = Some (Participated.create true)
            ; no_show = Some (NoShow.create false)
            } )
        |> Pool_event.assignment
      ]
      |> handle
    in
    let%lwt assignment = find pool assignment.id ||> get_exn in
    Cqrs_command.Session_command.Close.handle
      experiment
      session
      []
      [ assignment, IncrementParticipationCount.create true, None ]
    |> get_exn
    |> handle
  in
  (* Merge contacts: override b with a *)
  let%lwt contact_a = Contact.find pool (Contact.id contact_a) ||> get_exn in
  let duplicate =
    Duplicate_contacts.
      { id = Id.create ()
      ; contact_a
      ; contact_b
      ; score = 1.0
      ; ignored = Ignored.create false
      }
  in
  let urlencoded =
    make_urlencoded
      ~email:contact_b
      ~firstname:contact_b
      ~lastname:contact_b
      ~cell_phone:contact_b
      ~language:contact_b
      []
  in
  let%lwt () =
    Command.Merge.handle urlencoded duplicate [] ([], [])
    |> Lwt_result.lift
    >>= merge pool
    ||> get_exn
  in
  let open Contact in
  let open Alcotest in
  let%lwt result = find pool (id contact_b) ||> get_exn in
  check int "no invites" 1 (num_invitations result |> NumberOfInvitations.value);
  check int "no assignments" 1 (num_assignments result |> NumberOfAssignments.value);
  check int "participations" 1 (num_participations result |> NumberOfParticipations.value);
  check int "no shows" 0 (num_no_shows result |> NumberOfNoShows.value);
  let%lwt is_enrolled =
    Assignment.assignment_to_experiment_exists pool experiment.Experiment.id contact_b
  in
  check bool "contact_b is enrolled" true is_enrolled;
  Lwt.return_unit
;;
