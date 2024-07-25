module Conformist = Pool_conformist

let src = Logs.Src.create "common.cqrs"

module ResetPassword : sig
  include Common.CommandSig

  type t = Pool_user.EmailAddress.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Email.dispatch
    -> (Pool_event.t list, 'a) result

  val decode : Conformist.input -> (t, Pool_message.Error.t) result
  val effects : Role.Target.t -> Pool_user.t -> Guard.ValidationSet.t
end = struct
  type t = Pool_user.EmailAddress.t

  let command m = m

  let schema =
    Conformist.(make Field.[ Pool_user.EmailAddress.schema () ] command)
  ;;

  let handle ?tags reset_email =
    Logs.info ~src (fun m -> m ?tags "Handle command ResetPassword");
    Ok [ Email.sent reset_email |> Pool_event.email ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects role user =
    let open Guard in
    let target_id =
      user.Pool_user.id |> Guard.Uuid.target_of Pool_user.Id.value
    in
    ValidationSet.one_of_tuple (Permission.Update, role, Some target_id)
  ;;
end
