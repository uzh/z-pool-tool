module Conformist = Pool_common.Utils.PoolConformist
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
    -> ?password_policy:
         (User.Password.t -> (unit, Pool_common.Message.error) result)
    -> ?user_id:Id.t
    -> ?terms_accepted_at:User.TermsAccepted.t option
    -> Email.Token.t
    -> User.EmailAddress.t
    -> Sihl_email.t
    -> Pool_common.Language.t option
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
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
          ; User.Password.schema ()
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ]
        command)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    ?allowed_email_suffixes
    ?password_policy
    ?(user_id = Id.create ())
    ?(terms_accepted_at = Some (User.TermsAccepted.create_now ()))
    token
    unverified_email
    verification_email
    default_language
    command
    =
    Logs.info ~src (fun m -> m "Handle command SignUp" ~tags);
    let open CCResult in
    let* () = User.Password.validate ?password_policy command.password in
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
    Ok
      [ Contact.Created contact |> Pool_event.contact
      ; Email.Created (unverified_email, token, user_id)
        |> Pool_event.email_verification
      ; Email.Sent verification_email |> Pool_event.email
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = [ `Create, `TargetEntity `Contact ]
end

module DeleteUnverified : sig
  include Common.CommandSig

  val handle
    :  ?tags:Logs.Tag.set
    -> Contact.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.t -> Guard.Authorizer.effect list
end = struct
  type t

  let handle ?(tags = Logs.Tag.empty) contact =
    Logs.info ~src (fun m -> m "Handle command DeleteUnverified" ~tags);
    if contact.Contact.email_verified |> CCOption.is_some
    then Error Pool_common.Message.EmailDeleteAlreadyVerified
    else Ok [ Contact.UnverifiedDeleted contact |> Pool_event.contact ]
  ;;

  let effects contact =
    [ ( `Delete
      , `Target (Contact.id contact |> Guard.Uuid.target_of Pool_common.Id.value)
      )
    ]
  ;;
end

module Update : sig
  include Common.CommandSig

  type t = Custom_field.PartialUpdate.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_context.user
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = Custom_field.PartialUpdate.t

  let handle ?(tags = Logs.Tag.empty) user contact (field : t) =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    Ok
      [ Custom_field.PartialUpdate (field, contact, user)
        |> Pool_event.custom_field
      ]
  ;;

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Contact.Id.value)
    ; `Update, `TargetEntity `Contact
    ]
  ;;
end

module ClearAnswer : sig
  include Common.CommandSig

  type t = Contact.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Custom_field.Public.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = Contact.t

  let handle ?(tags = Logs.Tag.empty) field contact =
    Logs.info ~src (fun m -> m "Handle command ClearAnswer" ~tags);
    Ok
      [ Custom_field.AdminAnswerCleared (field, Contact.id contact)
        |> Pool_event.custom_field
      ]
  ;;

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Contact.Id.value)
    ; `Update, `TargetEntity `Contact
    ]
  ;;
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
    -> ?password_policy:
         (User.Password.t -> (unit, Pool_common.Message.error) result)
    -> Contact.t
    -> Sihl_email.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Contact.t -> Guard.Authorizer.effect list
end = struct
  type t =
    { current_password : User.Password.t
    ; new_password : User.Password.t
    ; password_confirmation : User.PasswordConfirmed.t
    }

  let command current_password new_password password_confirmation =
    { current_password; new_password; password_confirmation }
  ;;

  let schema =
    let open Pool_common.Message.Field in
    Conformist.(
      make
        Field.
          [ User.Password.schema ~field:CurrentPassword ()
          ; User.Password.schema ~field:NewPassword ()
          ; User.PasswordConfirmed.schema ()
          ]
        command)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    ?password_policy
    contact
    notification
    command
    =
    Logs.info ~src (fun m -> m "Handle command UpdatePassword" ~tags);
    let open CCResult in
    let* () =
      User.Password.validate_current_password
        contact.Contact.user
        command.current_password
    in
    let* () = User.Password.validate ?password_policy command.new_password in
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

  let effects subject =
    [ ( `Update
      , `Target (Contact.id subject |> Guard.Uuid.target_of Pool_common.Id.value)
      )
    ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module RequestEmailValidation : sig
  include Common.CommandSig

  type t = Pool_user.EmailAddress.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> Email.Token.t
    -> Sihl_email.t
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.t -> Guard.Authorizer.effect list
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
      [ Email.Created (email, token, Contact.id contact)
        |> Pool_event.email_verification
      ; Email.Sent verification_email |> Pool_event.email
      ]
  ;;

  let effects subject =
    [ ( `Update
      , `Target (Contact.id subject |> Guard.Uuid.target_of Pool_common.Id.value)
      )
    ]
  ;;
end

module UpdateEmail : sig
  type t = Email.unverified Email.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.t -> Pool_tenant.t -> Guard.Authorizer.effect list
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

  let effects contact tenant =
    [ ( `Update
      , `Target (Contact.id contact |> Guard.Uuid.target_of Pool_common.Id.value)
      )
    ; ( `Update
      , `Target
          (tenant.Pool_tenant.id |> Guard.Uuid.target_of Pool_tenant.Id.value) )
    ]
  ;;
end

module AcceptTermsAndConditions : sig
  include Common.CommandSig

  val handle
    :  ?tags:Logs.Tag.set
    -> Contact.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.t -> Guard.Authorizer.effect list
end = struct
  type t

  let handle ?(tags = Logs.Tag.empty) contact =
    Logs.info ~src (fun m -> m "Handle command AcceptTermsAndCondition" ~tags);
    Ok [ Contact.TermsAccepted contact |> Pool_event.contact ]
  ;;

  let effects contact =
    [ ( `Update
      , `Target
          (Guard.Uuid.Target.of_string_exn contact.Contact.user.Sihl_user.id) )
    ]
  ;;
end

module SendProfileUpdateTrigger : sig
  include Common.CommandSig

  type t =
    { contacts : Contact.t list
    ; emails : Sihl_email.t list
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.t -> Guard.Authorizer.effect list
end = struct
  type t =
    { contacts : Contact.t list
    ; emails : Sihl_email.t list
    }

  let handle ?(tags = Logs.Tag.empty) ({ contacts; emails } : t) =
    Logs.info ~src (fun m -> m "Handle command SendProfileUpdateTrigger" ~tags);
    Ok
      [ Contact.ProfileUpdateTriggeredAtUpdated contacts |> Pool_event.contact
      ; Email.BulkSent emails |> Pool_event.email
      ]
  ;;

  let effects contact =
    [ ( `Update
      , `Target
          (Guard.Uuid.Target.of_string_exn contact.Contact.user.Sihl_user.id) )
    ]
  ;;
end

module SendRegistrationAttemptNotifitacion : sig
  include Common.CommandSig with type t = Contact.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> Sihl_email.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
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

  let effects = []
end

module Verify = struct
  (* TODO issue #90 step 2 *)
  (* TODO Verify the contact itself with ID/Pass *)
end

module ToggleDisable = struct
  (* TODO issue #90 step 2 *)
  (* TODO Toggle disable command*)
end
