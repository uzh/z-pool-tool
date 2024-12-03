module Conformist = Pool_conformist

let src = Logs.Src.create "custom_field_settings.cqrs"

module UpdateVisibilitySettings : sig
  include Common.CommandSig

  type t = Custom_field.t list

  val handle
    :  ?tags:Logs.Tag.set
    -> selected:string list
    -> [< `Close | `Detail ]
    -> t
    -> unit
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Guard.ValidationSet.t
end = struct
  type t = Custom_field.t list

  let handle ?(tags = Logs.Tag.empty) ~selected setting fields () =
    Logs.info ~src (fun m -> m "Handle command UpdateVisibilitySettings" ~tags);
    let open Custom_field in
    let getter, setter =
      match setting with
      | `Close -> show_on_session_close_page, set_show_on_session_close_page
      | `Detail -> show_on_session_detail_page, set_show_on_session_detail_page
    in
    let active, inactive =
      CCList.partition_filter_map
        (fun field ->
           let active = getter field in
           CCList.find_opt
             (fun selected_id ->
                selected_id |> Id.of_string |> Id.equal (id field))
             selected
           |> function
           | Some (_ : string) when not active ->
             `Left (Updated (field, setter true field))
           | None when active -> `Right (Updated (field, setter false field))
           | Some (_ : string) | None -> `Drop)
        fields
    in
    active @ inactive |> CCList.map Pool_event.custom_field |> CCResult.return
  ;;

  let effects = Custom_field.Guard.Access.create
end
