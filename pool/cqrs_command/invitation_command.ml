let src = Logs.Src.create "invitation.cqrs"

let contact_partition invited =
  CCList.partition (fun contact ->
    CCList.mem ~eq:Contact.Id.equal (Contact.id contact) invited)
;;

let contact_update_on_invitation_sent contacts =
  let open CCFun in
  let open CCList in
  contacts
  >|= Contact_counter.update_on_invitation_sent %> Contact.updated %> Pool_event.contact
;;

module Create : sig
  include Common.CommandSig

  type t =
    { experiment : Experiment.t
    ; mailing : Mailing.t option
    ; contacts : Contact.t list
    ; invited_contacts : Contact.Id.t list
    ; create_message : Invitation.t -> (Email.dispatch, Pool_message.Error.t) result
    }

  val handle
    :  ?ids:Pool_common.Id.t list
    -> ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { experiment : Experiment.t
    ; mailing : Mailing.t option
    ; contacts : Contact.t list
    ; invited_contacts : Contact.Id.t list
    ; create_message : Invitation.t -> (Email.dispatch, Pool_message.Error.t) result
    }

  let handle
        ?(ids = [])
        ?(tags = Logs.Tag.empty)
        { invited_contacts; contacts; create_message; experiment; mailing }
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let open CCFun.Infix in
    let open CCList in
    let errors, contacts = contact_partition invited_contacts contacts in
    let errors = errors >|= Contact.(id %> Id.value) in
    let invitations = mapi (fun i -> Invitation.create ?id:(nth_opt ids i)) contacts in
    let emails = invitations >|= create_message in
    if is_empty errors |> not
    then Error Pool_message.(Error.AlreadyInvitedToExperiment errors)
    else (
      match all_ok emails with
      | Ok emails when is_empty emails -> Ok []
      | Ok emails ->
        Ok
          ([ Invitation.(Created { invitations; mailing; experiment })
             |> Pool_event.invitation
           ]
           @ (Email.bulksent_opt emails |> Pool_event.(map email))
           @ contact_update_on_invitation_sent contacts)
      | Error err -> Error err)
  ;;

  let effects = Invitation.Guard.Access.create
end

module Resend : sig
  include Common.CommandSig

  type t = Invitation.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?mailing_id:Mailing.Id.t
    -> (t -> (Email.dispatch, Pool_message.Error.t) result)
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Invitation.t

  let handle ?(tags = Logs.Tag.empty) ?mailing_id create_email (invitation : t) =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Resend" ~tags);
    let* email = create_email invitation in
    Ok
      [ Invitation.Resent (invitation, mailing_id) |> Pool_event.invitation
      ; Email.sent email |> Pool_event.email
      ]
  ;;

  let effects = Invitation.Guard.Access.update
end
