module Conformist = Pool_conformist
module User = Pool_user
module Id = Pool_common.Id

let src = Logs.Src.create "contact.cqrs"

module SignUp : sig
  include Common.CommandSig

  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?user_id:Contact.Id.t
    -> ?terms_accepted_at:User.TermsAccepted.t option
    -> Custom_field.Public.t list
    -> Email.Token.t
    -> User.EmailAddress.t
    -> Email.job
    -> Pool_common.Language.t option
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Guard.ValidationSet.t
end = struct
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t [@opaque]
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    }

  let command email password firstname lastname =
    { email; password; firstname; lastname }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ User.EmailAddress.schema ()
          ; User.Password.(schema create ())
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ]
        command)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    ?allowed_email_suffixes
    ?(user_id = Contact.Id.create ())
    ?(terms_accepted_at = Some (User.TermsAccepted.create_now ()))
    custom_fields
    token
    unverified_email
    verification_email
    default_language
    command
    =
    Logs.info ~src (fun m -> m "Handle command SignUp" ~tags);
    let open CCResult in
    let* () = User.EmailAddress.validate allowed_email_suffixes command.email in
    let contact =
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
    Conformist.decode_and_validate schema data
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

module UpdatePassword : sig
  include Common.CommandSig

  type t =
    { current_password : User.Password.t
    ; new_password : User.Password.t
    ; password_confirmation : User.PasswordConfirmed.t
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> Contact.t
    -> Email.job
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t =
    { current_password : User.Password.t [@opaque]
    ; new_password : User.Password.t [@opaque]
    ; password_confirmation : User.PasswordConfirmed.t [@opaque]
    }

  let command current_password new_password password_confirmation =
    { current_password; new_password; password_confirmation }
  ;;

  let schema =
    let open Pool_message.Field in
    Conformist.(
      make
        Field.
          [ User.Password.(schema ~field:CurrentPassword create_unvalidated ())
          ; User.Password.(schema ~field:NewPassword create ())
          ; User.PasswordConfirmed.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) contact notification command =
    Logs.info ~src (fun m -> m "Handle command UpdatePassword" ~tags);
    let open CCResult in
    let* () =
      User.validate_current_password
        contact.Contact.user
        command.current_password
    in
    let* () =
      User.Password.validate_password_confirmation
        command.new_password
        command.password_confirmation
    in
    Ok
      [ Contact.PasswordUpdated
          ( contact
          , command.current_password
          , command.new_password
          , command.password_confirmation )
        |> Pool_event.contact
      ; Email.Sent notification |> Pool_event.email
      ]
  ;;

  let effects = Contact.Guard.Access.update

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;
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
    let* () = User.EmailAddress.validate allowed_email_suffixes email in
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
      User.EmailAddress.validate allowed_email_suffixes (Email.address email)
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
    with type t = Contact.t * User.CellPhone.t * Pool_common.VerificationCode.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Contact.t * User.CellPhone.t * Pool_common.VerificationCode.t

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
  include Common.CommandSig with type t = Contact.t * User.CellPhone.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Contact.t * User.CellPhone.t

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
