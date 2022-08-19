module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user
module Id = Pool_common.Id

module SignUp : sig
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; recruitment_channel : Contact.RecruitmentChannel.t
    }

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:
         (User.Password.t -> (unit, Pool_common.Message.error) result)
    -> ?user_id:Id.t
    -> ?terms_accepted_at:User.TermsAccepted.t
    -> Pool_common.Language.t option
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; recruitment_channel : Contact.RecruitmentChannel.t
    }

  let command email password firstname lastname recruitment_channel =
    { email; password; firstname; lastname; recruitment_channel }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ User.EmailAddress.schema ()
          ; User.Password.schema ()
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ; Contact.RecruitmentChannel.schema ()
          ]
        command)
  ;;

  let handle
      ?allowed_email_suffixes
      ?password_policy
      ?(user_id = Id.create ())
      ?(terms_accepted_at = User.TermsAccepted.create_now ())
      default_language
      command
    =
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
        ; recruitment_channel = command.recruitment_channel
        ; terms_accepted_at
        ; language = default_language
        }
    in
    Ok
      [ Contact.Created contact |> Pool_event.contact
      ; Email.Created
          ( command.email
          , user_id
          , command.firstname
          , command.lastname
          , default_language |> CCOption.get_or ~default:Pool_common.Language.En
          )
        |> Pool_event.email_verification
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = [ `Create, `Role `Contact ]
end

module DeleteUnverified : sig
  val handle
    :  Contact.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.t -> Ocauth.Authorizer.effect list
end = struct
  let handle contact =
    if contact.Contact.email_verified |> User.EmailVerified.is_some
    then Error Pool_common.Message.EmailDeleteAlreadyVerified
    else Ok [ Contact.UnverifiedDeleted contact |> Pool_event.contact ]
  ;;

  let effects contact =
    [ `Delete, `Uniq (Contact.id contact |> Pool_common.Id.to_uuidm) ]
  ;;
end

module Update : sig
  type t =
    { firstname : User.Firstname.t option
    ; lastname : User.Lastname.t option
    ; paused : User.Paused.t option
    ; language : Pool_common.Language.t option
    ; experiment_type_preference : Pool_common.ExperimentType.t option
    }

  val handle
    :  Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Contact.t -> Ocauth.Authorizer.effect list

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
end = struct
  type t =
    { firstname : User.Firstname.t option
    ; lastname : User.Lastname.t option
    ; paused : User.Paused.t option
    ; language : Pool_common.Language.t option
    ; experiment_type_preference : Pool_common.ExperimentType.t option
    }

  let command firstname lastname paused language experiment_type_preference =
    { firstname; lastname; paused; language; experiment_type_preference }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Conformist.optional @@ User.Firstname.schema ()
          ; Conformist.optional @@ User.Lastname.schema ()
          ; Conformist.optional @@ User.Paused.schema ()
          ; Conformist.optional @@ Pool_common.Language.schema ()
          ; Conformist.optional @@ Pool_common.ExperimentType.schema ()
          ]
        command)
  ;;

  let handle contact (command : t) =
    Contact.
      [ command.firstname |> CCOption.map (firstnameupdated contact)
      ; command.lastname |> CCOption.map (lastnameupdated contact)
      ; command.paused |> CCOption.map (pausedupdated contact)
      ; command.language |> CCOption.map (languageupdated contact)
      ]
    |> CCList.filter_map CCFun.id
    |> CCList.map Pool_event.contact
    |> CCResult.pure
  ;;

  let effects pool subject =
    [ `Update, `Uniq (Contact.id subject |> Pool_common.Id.to_uuidm)
    ; `Update, `Uniq (Pool_tenant.id pool |> Pool_common.Id.to_uuidm)
    ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module UpdatePassword : sig
  type t =
    { current_password : User.Password.t
    ; new_password : User.Password.t
    ; password_confirmation : User.PasswordConfirmed.t
    }

  val handle
    :  ?password_policy:
         (User.Password.t -> (unit, Pool_common.Message.error) result)
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Contact.t -> Ocauth.Authorizer.effect list

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
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

  let handle ?password_policy contact command =
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
      ; Email.ChangedPassword
          ( contact.Contact.user
          , contact.Contact.language
            |> CCOption.get_or ~default:Pool_common.Language.En )
        |> Pool_event.email
      ]
  ;;

  let effects tenant subject =
    [ `Update, `Uniq (Contact.id subject |> Pool_common.Id.to_uuidm)
    ; `Update, `Uniq (Pool_tenant.id tenant |> Pool_common.Id.to_uuidm)
    ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module RequestEmailValidation : sig
  type t = Pool_user.EmailAddress.t

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Contact.t -> Ocauth.Authorizer.effect list
end = struct
  type t = Pool_user.EmailAddress.t

  let handle ?allowed_email_suffixes contact email =
    let open CCResult in
    let* () = User.EmailAddress.validate allowed_email_suffixes email in
    Ok
      [ Email.Updated
          ( email
          , contact.Contact.user
          , contact.Contact.language
            |> CCOption.get_or ~default:Pool_common.Language.En )
        |> Pool_event.email_verification
      ]
  ;;

  let effects tenant subject =
    [ `Update, `Uniq (Contact.id subject |> Pool_common.Id.to_uuidm)
    ; `Update, `Uniq (Pool_tenant.id tenant |> Pool_common.Id.to_uuidm)
    ]
  ;;
end

module UpdateEmail : sig
  type t = Email.unverified Email.t

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.t -> Pool_tenant.t -> Ocauth.Authorizer.effect list
end = struct
  type t = Email.unverified Email.t

  let handle ?allowed_email_suffixes contact email =
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
    [ `Update, `Uniq (Contact.id contact |> Pool_common.Id.to_uuidm)
    ; `Update, `Uniq (Pool_common.Id.to_uuidm tenant.Pool_tenant.id)
    ]
  ;;
end

module AcceptTermsAndConditions : sig
  val handle
    :  Contact.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.t -> Ocauth.Authorizer.effect list
end = struct
  let handle contact =
    Ok [ Contact.TermsAccepted contact |> Pool_event.contact ]
  ;;

  let effects contact =
    [ ( `Update
      , `Uniq (Ocauth.Uuid.of_string_exn contact.Contact.user.Sihl_user.id) )
    ]
  ;;
end

module VerifyEmail : sig
  type t = { email : Email.unverified Email.t }

  val handle
    :  Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Contact.t -> Ocauth.Authorizer.effect list
end = struct
  type t = { email : Email.unverified Email.t }

  let handle contact command =
    Ok
      [ Contact.EmailVerified contact |> Pool_event.contact
      ; Email.EmailVerified command.email |> Pool_event.email_verification
      ]
  ;;

  let effects _contact = Utils.todo [%here]
end

module SendProfileUpdateTrigger : sig
  type t =
    { contacts : Contact.t list
    ; emails : Sihl_email.t list
    }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val effects : Contact.t -> Ocauth.Authorizer.effect list
end = struct
  type t =
    { contacts : Contact.t list
    ; emails : Sihl_email.t list
    }

  let handle ({ contacts; emails } : t) =
    Ok
      [ Contact.ProfileUpdateTriggeredAtUpdated contacts |> Pool_event.contact
      ; Email.BulkSent emails |> Pool_event.email
      ]
  ;;

  let effects contact =
    [ ( `Update
      , `Uniq (Ocauth.Uuid.of_string_exn contact.Contact.user.Sihl_user.id) )
    ]
  ;;
end

module Verify = struct
  (* TODO issue #90 step 2 *)
  (* TODO Verify the contact itself with ID/Pass *)
end

module ToggleDisable = struct
  (* TODO issue #90 step 2 *)
  (* TODO Toggle disable command*)
end
