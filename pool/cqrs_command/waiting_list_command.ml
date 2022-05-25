module Conformist = Pool_common.Utils.PoolConformist

module Create : sig
  type t = Waiting_list.create

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> bool Lwt.t
end = struct
  type t = Waiting_list.create

  let handle (command : Waiting_list.create) =
    if command.Waiting_list.experiment.Experiment.Public.waiting_list_disabled
    then Error Pool_common.Message.NotEligible
    else Ok [ Waiting_list.Created command |> Pool_event.waiting_list ]
  ;;

  let can user =
    Permission.can user ~any_of:[ Permission.Create Permission.Waiting_list ]
  ;;
end

module Update : sig
  type t = Waiting_list.update

  val handle
    :  Waiting_list.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can : Sihl_user.t -> bool Lwt.t
end = struct
  type t = Waiting_list.update

  let command comment = Waiting_list.{ comment }

  let schema =
    Conformist.(
      make
        Field.[ Conformist.optional @@ Waiting_list.Comment.schema () ]
        command)
  ;;

  let handle waiting_list (command : t) =
    Ok
      [ Waiting_list.Updated (command, waiting_list) |> Pool_event.waiting_list
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let can user =
    Permission.can user ~any_of:[ Permission.Create Permission.Waiting_list ]
  ;;
end

module Destroy : sig
  type t = Waiting_list.t

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Sihl_user.t -> bool Lwt.t
end = struct
  type t = Waiting_list.t

  let handle m = Ok [ Waiting_list.Deleted m |> Pool_event.waiting_list ]

  let can user =
    Permission.can user ~any_of:[ Permission.Create Permission.Waiting_list ]
  ;;
end
