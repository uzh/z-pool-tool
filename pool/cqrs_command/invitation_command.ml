let src = Logs.Src.create "invitation.cqrs"

let contact_partition invited =
  CCList.partition (fun contact ->
    CCList.mem ~eq:Pool_common.Id.equal (Contact.id contact) invited)
;;

let prepare_emails experiment create_message =
  let open CCResult in
  CCList.map (fun contact ->
    contact
    |> create_message
    >|= fun msg -> msg, experiment.Experiment.smtp_auth_id)
;;

let contact_update_on_invitation_sent contacts =
  let open CCFun in
  let open CCList in
  contacts
  >|= Contact_counter.update_on_invitation_sent
      %> Contact.updated
      %> Pool_event.contact
;;

module Create : sig
  include Common.CommandSig

  type t =
    { experiment : Experiment.t
    ; mailing : Mailing.t option
    ; contacts : Contact.t list
    ; invited_contacts : Pool_common.Id.t list
    ; create_message :
        Contact.t -> (Sihl_email.t, Pool_common.Message.error) result
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { experiment : Experiment.t
    ; mailing : Mailing.t option
    ; contacts : Contact.t list
    ; invited_contacts : Pool_common.Id.t list
    ; create_message :
        Contact.t -> (Sihl_email.t, Pool_common.Message.error) result
    }

  let handle
    ?(tags = Logs.Tag.empty)
    { invited_contacts; contacts; create_message; experiment; mailing }
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let open CCFun in
    let errors, contacts = contact_partition invited_contacts contacts in
    let errors = CCList.map (Contact.id %> Pool_common.Id.value) errors in
    let emails = prepare_emails experiment create_message contacts in
    if CCList.is_empty errors |> not
    then Error Pool_common.Message.(AlreadyInvitedToExperiment errors)
    else (
      match CCList.all_ok emails with
      | Ok emails when CCList.is_empty emails -> Ok []
      | Ok emails ->
        Ok
          ([ Invitation.(Created { contacts; mailing; experiment })
             |> Pool_event.invitation
           ; Email.BulkSent emails |> Pool_event.email
           ]
           @ contact_update_on_invitation_sent contacts)
      | Error err -> Error err)
  ;;

  let effects = Invitation.Guard.Access.create
end

module Resend : sig
  include Common.CommandSig

  type t =
    { invitation : Invitation.t
    ; experiment : Experiment.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?mailing_id:Mailing.Id.t
    -> (Contact.t -> (Sihl_email.t, Pool_common.Message.error) result)
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { invitation : Invitation.t
    ; experiment : Experiment.t
    }

  let handle
    ?(tags = Logs.Tag.empty)
    ?mailing_id
    create_email
    ({ invitation; experiment } : t)
    =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Resend" ~tags);
    let* email = create_email invitation.Invitation.contact in
    Ok
      [ Invitation.Resent (invitation, mailing_id) |> Pool_event.invitation
      ; Email.Sent (email, experiment.Experiment.smtp_auth_id)
        |> Pool_event.email
      ]
  ;;

  let effects = Invitation.Guard.Access.update
end
