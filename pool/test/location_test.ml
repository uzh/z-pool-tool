module LocationCommand = Cqrs_command.Location_command
module Field = Pool_message.Field

module Data = struct
  module Location = struct
    let id = Pool_location.Id.create ()

    let name =
      Pool_location.Name.create "Online" |> Pool_common.Utils.get_or_failwith
    ;;

    let description : Pool_location.Description.t option = None
    let link = None
    let address = Pool_location.Address.Virtual
    let status = Pool_location.Status.Active
    let files = []

    let create =
      let open Pool_location in
      [ Field.(Name |> show), [ name |> Name.value ]
      ; Field.(Link |> show), [ link |> CCOption.map_or ~default:"" Link.value ]
      ; Field.(Virtual |> show), [ "true" ]
      ; Field.(Status |> show), [ status |> Status.show ]
      ]
    ;;
  end
end

let create_location () =
  let open Data.Location in
  Pool_location.
    { id
    ; name
    ; description
    ; link
    ; address
    ; status
    ; files
    ; created_at = Pool_common.CreatedAt.create_now ()
    ; updated_at = Pool_common.UpdatedAt.create_now ()
    }
;;

let create () =
  let open LocationCommand.Create in
  let location = create_location () in
  let events =
    Data.Location.create
    |> Http_utils.remove_empty_values
    |> decode None
    |> Pool_common.Utils.get_or_failwith
    |> handle ~id:Data.Location.id
  in
  let expected =
    Ok [ Pool_location.Created location |> Pool_event.pool_location ]
  in
  Test_utils.check_result expected events
;;

let update () =
  let open LocationCommand.Update in
  let open Pool_location in
  let admin =
    Test_utils.Model.create_admin ()
    |> Pool_context.get_admin_user
    |> Test_utils.get_or_failwith
  in
  let location = create_location () in
  let handle_update ?changelog_id urlencoded =
    urlencoded
    |> Http_utils.remove_empty_values
    |> decode None
    |> Pool_common.Utils.get_or_failwith
    |> handle ?changelog_id admin location
  in
  (* Update without change *)
  let events = Data.Location.create |> handle_update in
  let expected = Ok [ Updated location |> Pool_event.pool_location ] in
  let () = Test_utils.check_result expected events in
  (* Update with changed name *)
  let new_name = "new name" in
  let changelog_id = Changelog.Id.create () in
  let events =
    Data.Location.create
    |> CCList.Assoc.set ~eq:CCString.equal Field.(show Name) [ new_name ]
    |> handle_update ~changelog_id
  in
  let after = { location with name = Name.of_string new_name } in
  let changelog =
    VersionHistory.create
      ~id:changelog_id
      ~entity_uuid:(Id.to_common location.id)
      ~user_uuid:Admin.(admin |> id |> Id.to_common)
      ~before:location
      ~after
      ()
    |> CCOption.get_exn_or "Invalid changelog"
  in
  let expected =
    Ok
      [ Updated after |> Pool_event.pool_location
      ; Changelog.(changelog |> created |> Pool_event.changelog)
      ]
  in
  Test_utils.check_result expected events
;;
