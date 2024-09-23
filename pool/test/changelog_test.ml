(** TODO: Add testcases for
    - lists
    - filter
    - assoc lists (e.g. custom field names) *)

module Field = Pool_message.Field

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
  Alcotest.(check (option testable_changelog))
    "changelog is none"
    changelog
    None
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
    Assoc
      [ "name", Change (`String (Name.value location.name), `String new_name) ]
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
    { location with
      address = Physical { address with institution = Some institution }
    }
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
    Assoc
      [ "address", Assoc [ "institution", Change (`Null, `String institution) ]
      ]
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
  let before =
    And [ firstname "one"; firstname "two" ] |> create ~id:filter_id None
  in
  let after =
    And [ firstname "one"; firstname "three" ] |> create ~id:filter_id None
  in
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
                          [ ( "pred"
                            , Assoc
                                [ ( "value"
                                  , Assoc
                                      [ ( "str"
                                        , Change (`String "two", `String "three")
                                        )
                                      ] )
                                ] )
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
