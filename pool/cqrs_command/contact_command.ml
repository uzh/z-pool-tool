let src = Logs.Src.create "contact.cqrs"

module SignUp : sig
  include Common.CommandSig

  type t = User_command.create_user

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?user_id:Contact.Id.t
    -> ?terms_accepted_at:Pool_user.TermsAccepted.t option
    -> Custom_field.Public.t list
    -> Email.Token.t
    -> Pool_user.EmailAddress.t
    -> Email.job
    -> Pool_common.Language.t option
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Guard.ValidationSet.t
end = struct
  type t = User_command.create_user

  let handle
    ?(tags = Logs.Tag.empty)
    ?allowed_email_suffixes
    ?(user_id = Contact.Id.create ())
    ?(terms_accepted_at = Some (Pool_user.TermsAccepted.create_now ()))
    custom_fields
    token
    unverified_email
    verification_email
    default_language
    command
    =
    Logs.info ~src (fun m -> m "Handle command SignUp" ~tags);
    let open CCResult in
    let* () =
      Pool_user.EmailAddress.validate
        allowed_email_suffixes
        command.User_command.email
    in
    let contact =
      let open User_command in
      Contact.
        { user_id
        ; email = command.email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        ; terms_accepted_at
        ; language = default_language
        }
    in
    let custom_field_events =
      custom_fields
      |> CCList.map (fun field ->
        Custom_field.AnsweredOnSignup (field, user_id |> Contact.Id.to_common)
        |> Pool_event.custom_field)
    in
    Ok
      ([ Contact.Created contact |> Pool_event.contact
       ; Email.Created (unverified_email, token, user_id |> Contact.Id.to_user)
         |> Pool_event.email_verification
       ; Email.Sent verification_email |> Pool_event.email
       ]
       @ custom_field_events)
  ;;

  let decode data =
    Pool_conformist.decode_and_validate User_command.create_user_schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Contact.Guard.Access.create
end

module UpdateSignInCount : sig
  type t = Contact.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = Contact.t

  let handle ?(tags = Logs.Tag.empty) contact =
    Logs.info ~src (fun m -> m "Handle command UpdateSignInCount" ~tags);
    Ok [ Contact.SignInCounterUpdated contact |> Pool_event.contact ]
  ;;
end

module DeleteUnverified : sig
  include Common.CommandSig

  val handle
    :  ?tags:Logs.Tag.set
    -> Contact.t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t

  (* TODO: Also delete all custom_fields answers of this user *)
  let handle ?(tags = Logs.Tag.empty) contact =
    Logs.info ~src (fun m -> m "Handle command DeleteUnverified" ~tags);
    if contact.Contact.email_verified |> CCOption.is_some
    then Error Pool_message.Error.EmailDeleteAlreadyVerified
    else Ok [ Contact.UnverifiedDeleted contact |> Pool_event.contact ]
  ;;

  let effects = Contact.Guard.Access.update
end

module Update : sig
  include Common.CommandSig

  type t = Custom_field.PartialUpdate.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_context.user
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Custom_field.PartialUpdate.t

  let handle ?(tags = Logs.Tag.empty) user contact (field : t) =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    Ok
      [ Custom_field.PartialUpdate (field, contact, user)
        |> Pool_event.custom_field
      ]
  ;;

  let effects = Contact.Guard.Access.update
end

module ClearAnswer : sig
  include Common.CommandSig

  type t = Contact.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Custom_field.Public.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Contact.t

  let handle ?(tags = Logs.Tag.empty) field contact =
    Logs.info ~src (fun m -> m "Handle command ClearAnswer" ~tags);
    Ok
      [ Custom_field.AdminAnswerCleared
          (field, Contact.(id contact |> Id.to_common))
        |> Pool_event.custom_field
      ]
  ;;

  let effects = Contact.Guard.Access.update
end

module RequestEmailValidation : sig
  include Common.CommandSig

  type t = Pool_user.EmailAddress.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> Email.Token.t
    -> Email.job
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Pool_user.EmailAddress.t

  let handle
    ?(tags = Logs.Tag.empty)
    ?allowed_email_suffixes
    token
    verification_email
    (contact : Contact.t)
    email
    =
    Logs.info ~src (fun m -> m "Handle command RequestEmailValidation" ~tags);
    let open CCResult in
    let* () = Pool_user.EmailAddress.validate allowed_email_suffixes email in
    Ok
      [ Email.Created (email, token, Contact.(id contact |> Id.to_user))
        |> Pool_event.email_verification
      ; Email.Sent verification_email |> Pool_event.email
      ]
  ;;

  let effects = Contact.Guard.Access.update
end

module UpdateEmail : sig
  type t = Email.unverified Email.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Email.unverified Email.t

  let handle ?(tags = Logs.Tag.empty) ?allowed_email_suffixes contact email =
    Logs.info ~src (fun m -> m "Handle command UpdateEmail" ~tags);
    let open CCResult in
    let* () =
      Pool_user.EmailAddress.validate
        allowed_email_suffixes
        (Email.address email)
    in
    Ok
      [ Contact.EmailUpdated (contact, Email.address email)
        |> Pool_event.contact
      ; Email.EmailVerified email |> Pool_event.email_verification
      ]
  ;;

  let effects = Contact.Guard.Access.update
end

module AcceptTermsAndConditions : sig
  include Common.CommandSig

  val handle
    :  ?tags:Logs.Tag.set
    -> Contact.t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t

  let handle ?(tags = Logs.Tag.empty) contact =
    Logs.info ~src (fun m -> m "Handle command AcceptTermsAndCondition" ~tags);
    Ok [ Contact.TermsAccepted contact |> Pool_event.contact ]
  ;;

  let effects = Contact.Guard.Access.update
end

module SendProfileUpdateTrigger : sig
  include Common.CommandSig

  type t =
    { contacts : Contact.t list
    ; emails : Email.job list
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { contacts : Contact.t list
    ; emails : Email.job list
    }

  let handle ?(tags = Logs.Tag.empty) ({ contacts; emails } : t) =
    Logs.info ~src (fun m -> m "Handle command SendProfileUpdateTrigger" ~tags);
    Ok
      [ Contact.ProfileUpdateTriggeredAtUpdated contacts |> Pool_event.contact
      ; Email.BulkSent emails |> Pool_event.email
      ]
  ;;

  let effects = Contact.Guard.Access.update
end

module SendRegistrationAttemptNotifitacion : sig
  include Common.CommandSig with type t = Contact.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> Email.job
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Contact.t

  let handle ?(tags = Logs.Tag.empty) contact email =
    Logs.info ~src (fun m ->
      m "Handle command SendRegistrationAttemptNotifitacion" ~tags);
    Ok
      [ Email.Sent email |> Pool_event.email
      ; Contact.RegistrationAttemptNotificationSent contact
        |> Pool_event.contact
      ]
  ;;

  let effects = Contact.Guard.Access.update
end

module AddCellPhone : sig
  include
    Common.CommandSig
    with type t =
      Contact.t * Pool_user.CellPhone.t * Pool_common.VerificationCode.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Contact.t * Pool_user.CellPhone.t * Pool_common.VerificationCode.t

  let handle ?(tags = Logs.Tag.empty) (contact, cell_phone, token) =
    Logs.info ~src (fun m -> m "Handle command AddCellPhone" ~tags);
    Ok
      [ Contact.CellPhoneAdded (contact, cell_phone, token)
        |> Pool_event.contact
      ]
  ;;

  let effects = Contact.Guard.Access.update
end

module VerifyCellPhone : sig
  include Common.CommandSig with type t = Contact.t * Pool_user.CellPhone.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Contact.t * Pool_user.CellPhone.t

  let handle ?(tags = Logs.Tag.empty) (contact, cell_phone) =
    Logs.info ~src (fun m -> m "Handle command VerifyCellPhone" ~tags);
    Ok [ Contact.CellPhoneVerified (contact, cell_phone) |> Pool_event.contact ]
  ;;

  let effects = Contact.Guard.Access.update
end

module ResetCellPhoneVerification : sig
  include Common.CommandSig with type t = Contact.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Contact.t

  let handle ?(tags = Logs.Tag.empty) contact =
    Logs.info ~src (fun m ->
      m "Handle command ResetCellPhoneVerification" ~tags);
    Ok [ Contact.CellPhoneVerificationReset contact |> Pool_event.contact ]
  ;;

  let effects = Contact.Guard.Access.update
end

module TogglePaused : sig
  type t = Pool_user.Paused.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Pool_user.Paused.t

  let handle ?(tags = Logs.Tag.empty) contact paused =
    Logs.info ~src (fun m -> m "Handle command TogglePaused" ~tags);
    let increment = Pool_common.Version.increment in
    Ok
      [ Contact.Updated
          Contact.
            { contact with
              paused
            ; paused_version = contact.paused_version |> increment
            }
        |> Pool_event.contact
      ]
  ;;

  let effects = Contact.Guard.Access.update
end

module Verify = struct
  (* TODO issue #90 step 2 *)
  (* TODO Verify the contact itself with ID/Pass *)
end

module ToggleDisable = struct
  (* TODO issue #90 step 2 *)
  (* TODO Toggle disable command*)
end
