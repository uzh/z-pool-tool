include Entity
include Event
include Default

let prepare_email template email layout params =
  match Sihl.Configuration.read_string "SMTP_SENDER" with
  | None -> failwith "SMTP_SENDER not found in configuration"
  | Some sender ->
    let { email_subject; email_text; _ } = template in
    let mail =
      Sihl_email.
        { sender
        ; recipient = email
        ; subject = email_subject
        ; text = "" (* TODO: Plaintext *)
        ; html = Some email_text
        ; cc = []
        ; bcc = []
        }
    in
    let params = params @ Message_utils.layout_params layout in
    Sihl_email.Template.email_of_template mail params
;;

module AssignmentConfirmation = struct
  let email_params languages session =
    (CCList.map (fun lang ->
       ( Format.asprintf "sessionOverview%s" (Pool_common.Language.show lang)
       , Session.to_email_text lang session )))
      languages
  ;;

  let email_params_public_session languages session =
    (CCList.map (fun lang ->
       ( Format.asprintf "sessionOverview%s" (Pool_common.Language.show lang)
       , Session.public_to_email_text lang session )))
      languages
  ;;

  let template pool language =
    Repo.find_by_label pool language Label.AssignmentConfirmation
  ;;

  let base_params contact = [ "name", contact |> Contact.fullname ]

  let create pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params = base_params contact @ email_params [ language ] session in
    let layout = Message_utils.layout_from_tenant tenant in
    let email =
      contact |> Contact.email_address |> Pool_user.EmailAddress.value
    in
    prepare_email template email layout params |> Lwt_result.ok
  ;;

  let create_from_public_session pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params =
      base_params contact @ email_params_public_session [ language ] session
    in
    let layout = Message_utils.layout_from_tenant tenant in
    let email =
      contact |> Contact.email_address |> Pool_user.EmailAddress.value
    in
    prepare_email template email layout params |> Lwt_result.ok
  ;;
end
