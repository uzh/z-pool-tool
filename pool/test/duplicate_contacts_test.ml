module ContactRepo = Integration_utils.ContactRepo

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
    let%lwt () = run pool (id target |> Id.to_common) in
    find_by_contact pool target
    ||> fst
    ||> CCList.find_opt (fun { contact_a; contact_b; _ } ->
      Id.equal (id contact_a) (id comparison)
      || Id.equal (id contact_b) (id comparison))
  in
  let%lwt contact_1 = create_contact ~firstname:"John" ~lastname:"Doe" in
  let%lwt contact_2 = create_contact ~firstname:"John" ~lastname:"Doe" in
  let%lwt duplicate_score =
    find_duplicate ~target:contact_1 ~comparison:contact_2
    ||> CCOption.map (fun { score; _ } -> score)
  in
  check
    ~result:duplicate_score
    ~expected:(Some 1.0)
    "found duplicate with score 1";
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

let merge_contacts_command () =
  let open Cqrs_command.Duplicate_contacts_command.Merge in
  let open Test_utils.Model in
  let open Duplicate_contacts in
  let open Pool_message.Field in
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
  let contact_id contact =
    contact |> common_contact_id |> Pool_common.Id.value
  in
  let field_id field =
    let open Custom_field in
    Public.id field |> Id.to_common |> Pool_common.Id.value
  in
  let nr_siblings contact =
    Filter_test.CustomFieldData.NrOfSiblings.public
      ~entity_uuid:(common_contact_id contact)
      false
  in
  let contact_a = create_contact ~firstname:"John" ~lastname:"Doe" in
  let contact_b = create_contact ~firstname:"Foo" ~lastname:"Bar" in
  let duplicate = create_duplicate ~contact_a ~contact_b in
  let siblings_a = nr_siblings contact_a (Some 1) in
  let siblings_b = nr_siblings contact_b (Some 2) in
  let check_result urlencoded expected =
    let res = handle urlencoded duplicate ([ siblings_a ], [ siblings_b ]) in
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
    Ok
      { contact = contact_a
      ; merged_contact = contact_b
      ; kept_fields = [ siblings_a ]
      }
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
    Ok { contact; merged_contact = contact_a; kept_fields = [ siblings_a ] }
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
    Ok
      { contact = contact_a
      ; merged_contact = contact_b
      ; kept_fields = [ siblings_b ]
      }
  in
  check_result urlencoded expected
;;
