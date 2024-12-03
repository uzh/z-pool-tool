module Conformist = Pool_conformist

let src = Logs.Src.create "waiting_list.cqrs"

module Create : sig
  include Common.CommandSig with type t = Waiting_list.create

  val handle
    :  ?tags:Logs.Tag.set
    -> Email.dispatch
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Waiting_list.create

  let handle
        ?(tags = Logs.Tag.empty)
        confimration_email
        (command : Waiting_list.create)
    =
    let open Experiment.Public in
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    if
      command.Waiting_list.experiment
      |> direct_registration_disabled
      |> Experiment.DirectRegistrationDisabled.value
    then
      Ok
        [ Waiting_list.Created command |> Pool_event.waiting_list
        ; Email.sent confimration_email |> Pool_event.email
        ]
    else Error Pool_message.Error.NotEligible
  ;;

  let effects = Waiting_list.Guard.Access.create
end

module Update : sig
  include Common.CommandSig with type t = Waiting_list.update

  val handle
    :  ?tags:Logs.Tag.set
    -> Waiting_list.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Waiting_list.update

  let command admin_comment = { Waiting_list.admin_comment }

  let schema =
    Conformist.(
      make
        Field.[ Conformist.optional @@ Waiting_list.AdminComment.schema () ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) waiting_list (command : t) =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    Ok
      [ Waiting_list.Updated (command, waiting_list) |> Pool_event.waiting_list
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Waiting_list.Guard.Access.update
end

module Destroy : sig
  include Common.CommandSig with type t = Waiting_list.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Waiting_list.t

  let handle ?(tags = Logs.Tag.empty) m =
    Logs.info ~src (fun m -> m "Handle command Destroy" ~tags);
    Ok [ Waiting_list.Deleted m |> Pool_event.waiting_list ]
  ;;

  let effects = Waiting_list.Guard.Access.delete
end
