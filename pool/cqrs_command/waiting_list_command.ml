module Create : sig
  type t = Waiting_list.create

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> bool Lwt.t
end = struct
  type t = Waiting_list.create

  let handle command =
    Ok [ Waiting_list.Created command |> Pool_event.waiting_list ]
  ;;

  let can user =
    Permission.can user ~any_of:[ Permission.Create Permission.Waiting_list ]
  ;;
end

module Destroy : sig
  type t = Waiting_list.create

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> bool Lwt.t
end = struct
  type t = Waiting_list.create

  let handle m = Ok [ Waiting_list.Deleted m |> Pool_event.waiting_list ]

  let can user =
    Permission.can user ~any_of:[ Permission.Create Permission.Waiting_list ]
  ;;
end
