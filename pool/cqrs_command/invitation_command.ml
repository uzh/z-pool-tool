let src = Logs.Src.create "invitation.cqrs"

module Create : sig
  include Common.CommandSig

  type t =
    { experiment : Experiment.t
    ; contacts : Contact.t list
    ; invited_contacts : Pool_common.Id.t list
    ; create_message :
        Experiment.t
        -> Contact.t
        -> (Sihl_email.t, Pool_common.Message.error) result
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t =
    { experiment : Experiment.t
    ; contacts : Contact.t list
    ; invited_contacts : Pool_common.Id.t list
    ; create_message :
        Experiment.t
        -> Contact.t
        -> (Sihl_email.t, Pool_common.Message.error) result
    }

  let handle ?(tags = Logs.Tag.empty) (command : t) =
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
        (fun contact -> command.create_message command.experiment contact)
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
           ; Email.BulkSent emails |> Pool_event.email
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
    -> Sihl_email.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_common.Id.t -> Guard.Authorizer.effect list
end = struct
  type t =
    { invitation : Invitation.t
    ; experiment : Experiment.t
    }

  let handle ?(tags = Logs.Tag.empty) invitation_email (command : t) =
    Logs.info ~src (fun m -> m "Handle command Resend" ~tags);
    Ok
      [ Invitation.Resent command.invitation |> Pool_event.invitation
      ; Email.Sent invitation_email |> Pool_event.email
      ]
  ;;

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Pool_common.Id.value)
    ; `Update, `TargetEntity `Invitation
    ]
  ;;
end
