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
  | Updated of t * update
  | Deleted of t
[@@deriving eq, show, variants]

let insert_template ?(default = true) db_label t =
  let open Utils.Lwt_result.Infix in
  let insert = if default then Repo.insert_default_if_not_exists else Repo.insert in
  let%lwt () = insert db_label t in
  t
  |> Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx db_label)
  ||> Pool_common.Utils.get_or_failwith
  ||> fun (_ : Guard.Target.t) -> ()
;;

let handle_event ?user_uuid pool : event -> unit Lwt.t =
  let create_changelog before after =
    let open Version_history in
    insert pool ?user_uuid ~entity_uuid:before.id ~before ~after ()
  in
  function
  | Created template -> insert_template pool ~default:false template
  | Updated (template, { email_subject; email_text; plain_text; sms_text }) ->
    let updated = { template with email_subject; email_text; plain_text; sms_text } in
    let%lwt () = create_changelog template updated in
    updated |> Repo.update pool
  | Deleted { id; _ } -> Repo.delete pool id
;;
