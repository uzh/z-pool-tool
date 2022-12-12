let src = Logs.Src.create "invitation.cqrs"

let invitation_template_elements
  tenant
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
  let layout = Email.Helper.layout_from_tenant tenant in
  let open Experiment in
  match experiment.invitation_template with
  | Some template ->
    let subject =
      let open InvitationTemplate in
      template.subject |> Subject.value |> Email.CustomTemplate.Subject.string
    in
    let content =
      let open InvitationTemplate in
      template.text |> Text.value |> Email.CustomTemplate.Content.string
    in
    Ok Email.CustomTemplate.{ subject; content; layout }
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
        { subject = Subject.I18n subject; content = Content.I18n text; layout }
;;

module Create : sig
  include Common.CommandSig

  type t =
    { experiment : Experiment.t
    ; contacts : Contact.t list
    ; invited_contacts : Pool_common.Id.t list
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> Pool_tenant.t
    -> Pool_common.Language.t list
    -> (Pool_common.Language.t * (I18n.t * I18n.t)) list
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t =
    { experiment : Experiment.t
    ; contacts : Contact.t list
    ; invited_contacts : Pool_common.Id.t list
    }

  let handle
    ?(tags = Logs.Tag.empty)
    (command : t)
    tenant
    system_languages
    i18n_texts
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
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
    let emails =
      CCList.map
        (fun { Contact.user; language; _ } ->
          invitation_template_elements
            tenant
            system_languages
            i18n_texts
            command.experiment
            language
          |> CCResult.map (fun template ->
               ( user
               , [ ( "experimentDescription"
                   , command.experiment.Experiment.description
                     |> Experiment.Description.value )
                 ]
               , template )))
        contacts
    in
    if CCList.is_empty errors |> not
    then Error Pool_common.Message.(AlreadyInvitedToExperiment errors)
    else (
      match CCList.all_ok emails with
      | Ok emails when CCList.is_empty emails -> Ok []
      | Ok emails ->
        Ok
          ([ Invitation.Created (contacts, command.experiment)
             |> Pool_event.invitation
           ; Email.InvitationBulkSent emails |> Pool_event.email
           ]
          @ CCList.map
              (fun contact ->
                Contact.NumInvitationsIncreased contact |> Pool_event.contact)
              contacts)
      | Error err -> Error err)
  ;;

  let effects = [ `Create, `TargetEntity `Invitation ]
end

module Resend : sig
  include Common.CommandSig

  type t =
    { invitation : Invitation.t
    ; experiment : Experiment.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> Pool_tenant.t
    -> Pool_common.Language.t list
    -> (Pool_common.Language.t * (I18n.t * I18n.t)) list
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_common.Id.t -> Guard.Authorizer.effect list
end = struct
  type t =
    { invitation : Invitation.t
    ; experiment : Experiment.t
    }

  let handle
    ?(tags = Logs.Tag.empty)
    (command : t)
    tenant
    system_languages
    i18n_texts
    =
    Logs.info ~src (fun m -> m "Handle command Resend" ~tags);
    let open CCResult in
    let* email =
      invitation_template_elements
        tenant
        system_languages
        i18n_texts
        command.experiment
        command.invitation.Invitation.contact.Contact.language
    in
    Ok
      [ Invitation.Resent command.invitation |> Pool_event.invitation
      ; Email.InvitationSent
          ( command.invitation.Invitation.contact.Contact.user
          , [ ( "experimentDescription"
              , command.experiment.Experiment.description
                |> Experiment.Description.value )
            ]
          , email )
        |> Pool_event.email
      ]
  ;;

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Pool_common.Id.value)
    ; `Update, `TargetEntity `Invitation
    ]
  ;;
end
