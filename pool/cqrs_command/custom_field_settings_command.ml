module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "custom_field_settings.cqrs"

module UpdateVisibilitySettings : sig
  val handle
    :  ?tags:Logs.Tag.set
    -> active:Custom_field.t list
    -> inactive:Custom_field.t list
    -> unit
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.ValidationSet.t
end = struct
  let handle ?(tags = Logs.Tag.empty) ~active ~inactive () =
    Logs.info ~src (fun m -> m "Handle command UpdateVisibilitySettings" ~tags);
    let open Custom_field in
    let active = active |> CCList.map (set_show_on_session_close_page true) in
    let inactive =
      inactive |> CCList.map (set_show_on_session_close_page false)
    in
    active @ inactive
    |> CCList.map (fun field -> Updated field |> Pool_event.custom_field)
    |> CCResult.return
  ;;

  let effects = Custom_field.Guard.Access.create
end
