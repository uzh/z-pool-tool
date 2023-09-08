module LocationCommand = Cqrs_command.Location_command
module Field = Pool_common.Message.Field

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
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
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
