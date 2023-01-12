open Entity

type update =
  { email_subject : EmailSubject.t
  ; email_text : EmailText.t
  ; sms_text : SmsText.t
  }
[@@deriving eq, show]

type event =
  | Created of t
  | DefaultRestored of t list
  | Updated of t * update
[@@deriving eq, show]

let insert_template db_label t =
  let open Utils.Lwt_result.Infix in
  let%lwt () = Repo.insert db_label t in
  t
  |> Entity_guard.Target.to_authorizable ~ctx:(Pool_tenant.to_ctx db_label)
  ||> Pool_common.Utils.get_or_failwith
  ||> fun (_ : [> `MessageTemplate ] Guard.AuthorizableTarget.t) -> ()
;;

let handle_event pool : event -> unit Lwt.t = function
  | Created template -> insert_template pool template
  | DefaultRestored templates ->
    Lwt_list.iter_s (insert_template pool) templates
  | Updated (template, { email_subject; email_text; sms_text }) ->
    { template with email_subject; email_text; sms_text } |> Repo.update pool
;;
