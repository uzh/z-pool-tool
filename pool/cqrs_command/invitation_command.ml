let invitation_template_elements
    system_languages
    i18n_texts
    experiment
    contact_langauge
  =
  let open CCResult in
  let* default_language =
    system_languages
    |> CCList.head_opt
    |> CCOption.to_result Pool_common.Message.(Retrieve Field.Language)
  in
  let open Experiment in
  match experiment.invitation_template with
  | Some template ->
    let subject =
      let open InvitationTemplate in
      template.subject
      |> Subject.value
      |> fun s -> Email.CustomTemplate.Subject.String s
    in
    let content =
      let open InvitationTemplate in
      template.text
      |> Text.value
      |> fun s -> Email.CustomTemplate.Content.String s
    in
    Ok Email.CustomTemplate.{ subject; content }
  | None ->
    let language =
      contact_langauge
      |> CCOption.map_or ~default:default_language (fun l ->
             if CCList.mem ~eq:Pool_common.Language.equal l system_languages
             then l
             else default_language)
    in
    let* subject, text =
      CCList.Assoc.get ~eq:Pool_common.Language.equal language i18n_texts
      |> CCOption.to_result Pool_common.Message.(NotFound Field.I18n)
    in
    Ok
      Email.CustomTemplate.
        { subject = Subject.I18n subject; content = Content.I18n text }
;;

module Create : sig
  type t =
    { experiment : Experiment.t
    ; contacts : Contact.t list
    ; invited_contacts : Pool_common.Id.t list
    }

  val handle
    :  t
    -> Pool_common.Language.t list
    -> (Pool_common.Language.t * (I18n.t * I18n.t)) list
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { experiment : Experiment.t
    ; contacts : Contact.t list
    ; invited_contacts : Pool_common.Id.t list
    }

  let handle (command : t) system_languages i18n_texts =
    let open CCResult in
    let errors, contacts =
      CCList.partition
        (fun contact ->
          CCList.mem
            ~eq:Pool_common.Id.equal
            (Contact.id contact)
            command.invited_contacts)
        command.contacts
    in
    let errors = CCList.map Contact.fullname errors in
    let events, emails =
      CCList.fold_left
        (fun (events, emails) contact ->
          let create =
            Invitation.{ experiment = command.experiment; contact }
          in
          let event = Invitation.Created create |> Pool_event.invitation in
          let email =
            invitation_template_elements
              system_languages
              i18n_texts
              command.experiment
              contact.Contact.language
            |> CCResult.map (fun template -> contact, template)
          in
          event :: events, email :: emails)
        ([], [])
        contacts
    in
    if not (CCList.is_empty errors)
    then Error Pool_common.Message.(AlreadyInvitedToExperiment errors)
    else (
      match CCList.all_ok emails with
      | Ok emails ->
        Ok
          (events
          @ [ Invitation.InvitationsSent (command.experiment, emails)
              |> Pool_event.invitation
            ])
      | Error err -> Error err)
  ;;

  let can user command =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage
            (Permission.Experiment, Some command.experiment.Experiment.id)
        ; Permission.Create Permission.Invitation
        ]
  ;;
end

module Resend : sig
  val handle
    :  Invitation.resent
    -> Pool_common.Language.t list
    -> (Pool_common.Language.t * (I18n.t * I18n.t)) list
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Sihl_user.t -> Invitation.resent -> bool Lwt.t
end = struct
  let handle (command : Invitation.resent) system_languages i18n_texts =
    let open CCResult in
    let* email =
      invitation_template_elements
        system_languages
        i18n_texts
        command.Invitation.experiment
        command.Invitation.invitation.Invitation.contact.Contact.language
    in
    Ok [ Invitation.Resent (command, email) |> Pool_event.invitation ]
  ;;

  let can user Invitation.{ experiment; invitation } =
    Permission.can
      user
      ~any_of:
        [ Permission.Manage
            (Permission.Experiment, Some experiment.Experiment.id)
        ; Permission.Manage
            (Permission.Invitation, Some invitation.Invitation.id)
        ]
  ;;
end
