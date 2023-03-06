open Entity

type update =
  { email_subject : EmailSubject.t
  ; email_text : EmailText.t
  ; plain_text : PlainText.t
  ; sms_text : SmsText.t
  }
[@@deriving eq, show]

type event =
  | Created of t
  | DefaultRestored of t list
  | Updated of t * update
  | Deleted of t
[@@deriving eq, show, variants]

let insert_template ?(default = true) db_label t =
  let open Utils.Lwt_result.Infix in
  let insert =
    if default then Repo.insert_default_if_not_exists else Repo.insert
  in
  let%lwt () = insert db_label t in
  t
  |> Entity_guard.Target.to_authorizable ~ctx:(Pool_tenant.to_ctx db_label)
  ||> Pool_common.Utils.get_or_failwith
  ||> fun (_ : [> `MessageTemplate ] Guard.AuthorizableTarget.t) -> ()
;;

let handle_event pool : event -> unit Lwt.t = function
  | Created template -> insert_template pool ~default:false template
  | DefaultRestored templates ->
    Lwt_list.iter_s (insert_template pool) templates
  | Updated (template, { email_subject; email_text; plain_text; sms_text }) ->
    { template with email_subject; email_text; plain_text; sms_text }
    |> Repo.update pool
  | Deleted { id; _ } -> Repo.delete pool id
;;
