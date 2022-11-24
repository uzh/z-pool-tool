module Conformist = Pool_common.Utils.PoolConformist

let log_src = Logs.Src.create "common.cqrs"

module Logs = (val Logs.src_log log_src : Logs.LOG)

module ResetPassword = struct
  type t = Pool_user.EmailAddress.t

  let command m = m

  let schema =
    Conformist.(make Field.[ Pool_user.EmailAddress.schema () ] command)
  ;;

  let handle tenant language user =
    Logs.info (fun m -> m "Handle command ResetPassword");
    let layout = Email.Helper.layout_from_tenant tenant in
    Ok [ Email.ResetPassword (user, language, layout) |> Pool_event.email ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects () = Utils.todo [%here]
end
