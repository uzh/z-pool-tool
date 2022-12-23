open Entity

type update =
  { email_subject : EmailSubject.t
  ; email_text : EmailText.t
  ; sms_text : SmsText.t
  }
[@@deriving eq, show]

type event =
  | DefaultRestored of t list
  | Updated of t * update
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | DefaultRestored templates -> Lwt_list.iter_s (Repo.insert pool) templates
  | Updated (template, { email_subject; email_text; sms_text }) ->
    { template with email_subject; email_text; sms_text } |> Repo.update pool
;;
