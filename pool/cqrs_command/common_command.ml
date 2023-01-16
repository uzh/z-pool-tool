module Conformist = Pool_common.Utils.PoolConformist

let log_src = Logs.Src.create "common.cqrs"

module Logs = (val Logs.src_log log_src : Logs.LOG)

module ResetPassword : sig
  include Common.CommandSig

  type t = Pool_user.EmailAddress.t

  val handle : Sihl_email.t -> (Pool_event.t list, 'a) result
  val decode : Conformist.input -> (t, Conformist.error_msg) result
  val effects : Sihl_user.t -> Guard.Authorizer.effect list
end = struct
  type t = Pool_user.EmailAddress.t

  let command m = m

  let schema =
    Conformist.(make Field.[ Pool_user.EmailAddress.schema () ] command)
  ;;

  let handle reset_email =
    Logs.info (fun m -> m "Handle command ResetPassword");
    Ok [ Email.Sent reset_email |> Pool_event.email ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects user =
    [ `Update, `Target (user.Sihl_user.id |> Guard.Uuid.Target.of_string_exn) ]
  ;;
end
