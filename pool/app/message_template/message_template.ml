include Entity
include Event
include Default

let prepare_email language template email layout params =
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
        ; html = Some (Message_utils.combine_html language (Some email_subject))
        ; cc = []
        ; bcc = []
        }
    in
    let params =
      (("emailText", email_text) :: params) @ Message_utils.layout_params layout
    in
    Sihl_email.Template.email_of_template mail params
;;

module AssignmentConfirmation = struct
  let email_params lang session =
    "sessionOverview", Session.to_email_text lang session
  ;;

  let email_params_public_session lang session =
    "sessionOverview", Session.public_to_email_text lang session
  ;;

  let template pool language =
    Repo.find_by_label pool language Label.AssignmentConfirmation
  ;;

  let base_params contact = [ "name", contact |> Contact.fullname ]

  let create pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params = email_params language session :: base_params contact in
    let layout = Message_utils.layout_from_tenant tenant in
    let email =
      contact |> Contact.email_address |> Pool_user.EmailAddress.value
    in
    prepare_email language template email layout params |> Lwt_result.ok
  ;;

  let create_from_public_session pool language tenant session contact =
    let open Utils.Lwt_result.Infix in
    let* template = template pool language in
    let params =
      email_params_public_session language session :: base_params contact
    in
    let layout = Message_utils.layout_from_tenant tenant in
    let email =
      contact |> Contact.email_address |> Pool_user.EmailAddress.value
    in
    prepare_email language template email layout params |> Lwt_result.ok
  ;;
end
