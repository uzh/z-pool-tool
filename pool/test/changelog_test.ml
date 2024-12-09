module Field = Pool_message.Field
module SelectOption = Custom_field.SelectOption

let testable_changelog = Changelog.Write.(Alcotest.testable pp equal)
let id = Changelog.Id.create ()
let user_uuid = Pool_common.Id.create ()
let location = Test_utils.Model.create_location ()

let address =
  let open Pool_location.Address in
  Mail.
    { institution = None
    ; room = None
    ; building = None
    ; street = Street.of_string "street"
    ; zip = Zip.of_string "zip"
    ; city = City.of_string "city"
    }
;;

let model = Pool_message.Field.Location

let create_changelog changes =
  let open Changelog.Write in
  { id
  ; changes
  ; model
  ; entity_uuid = Pool_location.(Id.to_common location.id)
  ; user_uuid = Some user_uuid
  ; created_at = Pool_common.CreatedAt.create_now ()
  }
;;

let create_unchanged () =
  let changelog =
    let open Pool_location in
    VersionHistory.create
      ~id
      ~entity_uuid:(Id.to_common location.id)
      ~user_uuid
      ~before:location
      ~after:location
      ()
  in
  Alcotest.(check (option testable_changelog)) "changelog is none" changelog None
;;

let create () =
  let open Pool_location in
  let new_name = "new name" in
  let after = { location with name = Name.of_string new_name } in
  let changelog =
    VersionHistory.create
      ~id
      ~entity_uuid:(Id.to_common location.id)
      ~user_uuid
      ~before:location
      ~after
      ()
  in
  let expected =
    let open Changelog.Changes in
    Assoc [ "name", Change (`String (Name.value location.name), `String new_name) ]
    |> create_changelog
    |> CCOption.return
  in
  Alcotest.(check (option testable_changelog))
    "changelog contains name"
    changelog
    expected
;;

let create_nested () =
  let open Pool_location in
  let open Address in
  let location = { location with address = Address.Physical address } in
  let institution = "UZH" in
  let after =
    let open Mail in
    let institution = Institution.of_string institution in
    { location with address = Physical { address with institution = Some institution } }
  in
  let changelog =
    VersionHistory.create
      ~id
      ~entity_uuid:(Id.to_common location.id)
      ~user_uuid
      ~before:location
      ~after
      ()
  in
  let expected =
    let open Changelog.Changes in
    Assoc [ "address", Assoc [ "institution", Change (`Null, `String institution) ] ]
    |> create_changelog
    |> CCOption.return
  in
  Alcotest.(check (option testable_changelog))
    "changelog contains name"
    changelog
    expected
;;

let update_filter () =
  let open Filter in
  let equal = Operator.(Equality Equality.Equal) in
  let filter_id = Id.create () in
  let changelog_id = Changelog.Id.create () in
  let firstname name =
    Pred (Predicate.create Key.(Hardcoded Firstname) equal (Single (Str name)))
  in
  let before = And [ firstname "one"; firstname "two" ] |> create ~id:filter_id None in
  let after = And [ firstname "one"; firstname "three" ] |> create ~id:filter_id None in
  let changelog =
    VersionHistory.create
      ~id:changelog_id
      ~entity_uuid:(Id.to_common before.id)
      ~before
      ~after
      ()
  in
  let expected =
    let open Changelog in
    let changes =
      let open Changes in
      Assoc
        [ ( "query"
          , Assoc
              [ ( "and"
                , Assoc
                    [ ( "1"
                      , Assoc
                          [ ( "first_name"
                            , Assoc [ "value", Change (`String "two", `String "three") ] )
                          ] )
                    ] )
              ] )
        ]
    in
    Write.
      { id = changelog_id
      ; changes
      ; model = Pool_message.Field.Filter
      ; entity_uuid = Filter.Id.to_common before.id
      ; user_uuid = None
      ; created_at = Pool_common.CreatedAt.create_now ()
      }
    |> CCOption.return
  in
  Alcotest.(check (option testable_changelog))
    "changelog contains name"
    changelog
    expected
;;

let update_list_value () =
  let open Filter in
  let option_id = SelectOption.Id.create in
  let filter_id = Id.create () in
  let changelog_id = Changelog.Id.create () in
  let custom_field_id = Custom_field.Id.create () in
  let operator = Operator.(List ListM.ContainsSome) in
  let make_filter options =
    let options = List.map (fun option -> Option option) options in
    let query =
      Pred (Predicate.create Key.(CustomField custom_field_id) operator (Lst options))
    in
    create ~id:filter_id None query
  in
  let opt1 = option_id () in
  let opt2 = option_id () in
  let opt3 = option_id () in
  let before = make_filter [ opt1; opt2 ] in
  let make_changelog after =
    VersionHistory.create
      ~id:changelog_id
      ~entity_uuid:(Id.to_common filter_id)
      ~before
      ~after
      ()
  in
  let make_expected expected_changes =
    let open Changelog in
    let changes =
      let open Changes in
      Assoc
        [ ( "query"
          , Assoc
              [ Custom_field.Id.value custom_field_id, Assoc [ "value", expected_changes ]
              ] )
        ]
    in
    Write.
      { id = changelog_id
      ; changes
      ; model = Pool_message.Field.Filter
      ; entity_uuid = Filter.Id.to_common before.id
      ; user_uuid = None
      ; created_at = Pool_common.CreatedAt.create_now ()
      }
    |> CCOption.return
  in
  let run_test msg options expected_changes =
    let changelog = options |> make_filter |> make_changelog in
    let expected = expected_changes |> make_expected in
    Alcotest.(check (option testable_changelog)) msg expected changelog
  in
  let opt_id_json opt = `String (SelectOption.Id.value opt) in
  (* Add one option *)
  let options = [ opt1; opt2; opt3 ] in
  let expected_changes =
    let open Changelog.Changes in
    Change
      ( `List [ opt_id_json opt1; opt_id_json opt2 ]
      , `List [ opt_id_json opt1; opt_id_json opt2; opt_id_json opt3 ] )
  in
  let () = run_test "add an option" options expected_changes in
  (* Remove one option *)
  let options = [ opt1 ] in
  let expected_changes =
    let open Changelog.Changes in
    Change (`List [ opt_id_json opt1; opt_id_json opt2 ], `List [ opt_id_json opt1 ])
  in
  let () = run_test "remove an option" options expected_changes in
  (* Add and remove an option *)
  let options = [ opt1; opt3 ] in
  let expected_changes =
    let open Changelog.Changes in
    Change (opt_id_json opt2, opt_id_json opt3)
  in
  run_test "add and remove an option" options expected_changes
;;
