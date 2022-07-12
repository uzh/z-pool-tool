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
end

module DeleteUnverified : sig
  val handle
    :  Contact.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  let handle contact =
    if contact.Contact.email_verified |> User.EmailVerified.is_some
    then Error Pool_common.Message.EmailDeleteAlreadyVerified
    else Ok [ Contact.UnverifiedDeleted contact |> Pool_event.contact ]
  ;;
end

module Update : sig
  type t =
    { firstname : User.Firstname.t option
    ; lastname : User.Lastname.t option
    ; paused : User.Paused.t option
    ; language : Pool_common.Language.t option
    }

  val handle
    :  Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can
    :  Pool_tenant.Database.Label.t
    -> Sihl_user.t
    -> Contact.t
    -> bool Lwt.t
end = struct
  type t =
    { firstname : User.Firstname.t option
    ; lastname : User.Lastname.t option
    ; paused : User.Paused.t option
    ; language : Pool_common.Language.t option
    }

  let command firstname lastname paused language =
    { firstname; lastname; paused; language }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Conformist.optional @@ User.Firstname.schema ()
          ; Conformist.optional @@ User.Lastname.schema ()
          ; Conformist.optional @@ User.Paused.schema ()
          ; Conformist.optional @@ Pool_common.Language.schema ()
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

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let can pool user contact =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update (Permission.Contact, Some (contact |> Contact.id))
          ; Permission.Update (Permission.Tenant, Some tenant.Pool_tenant.id)
          ]
    in
    pool
    |> Pool_tenant.find_by_label
    |>> check_permission
    |> Lwt.map (CCResult.get_or ~default:false)
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

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can
    :  Pool_tenant.Database.Label.t
    -> Sihl_user.t
    -> Contact.t
    -> bool Lwt.t
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

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let can pool user contact =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update (Permission.Contact, Some (contact |> Contact.id))
          ; Permission.Update (Permission.Tenant, Some tenant.Pool_tenant.id)
          ]
    in
    pool
    |> Pool_tenant.find_by_label
    |>> check_permission
    |> Lwt.map (CCResult.get_or ~default:false)
  ;;
end

module RequestEmailValidation : sig
  type t = Pool_user.EmailAddress.t

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can
    :  Pool_tenant.Database.Label.t
    -> Sihl_user.t
    -> Contact.t
    -> bool Lwt.t
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

  let can pool user contact =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update (Permission.Contact, Some (Contact.id contact))
          ; Permission.Update (Permission.Tenant, Some tenant.Pool_tenant.id)
          ]
    in
    pool
    |> Pool_tenant.find_by_label
    |>> check_permission
    |> Lwt.map (CCResult.get_or ~default:false)
  ;;
end

module UpdateEmail : sig
  type t = Email.unverified Email.t

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Pool_database.Label.t -> Sihl_user.t -> Contact.t -> bool Lwt.t
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

  let can pool user contact =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update (Permission.Contact, Some (Contact.id contact))
          ; Permission.Update (Permission.Tenant, Some tenant.Pool_tenant.id)
          ]
    in
    pool
    |> Pool_tenant.find_by_label
    |>> check_permission
    |> Lwt.map (CCResult.get_or ~default:false)
  ;;
end

module AcceptTermsAndConditions : sig
  val handle
    :  Contact.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  let handle contact =
    Ok [ Contact.TermsAccepted contact |> Pool_event.contact ]
  ;;
end

module VerifyEmail : sig
  type t = { email : Email.unverified Email.t }

  val handle
    :  Contact.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = { email : Email.unverified Email.t }

  let handle contact command =
    Ok
      [ Contact.EmailVerified contact |> Pool_event.contact
      ; Email.EmailVerified command.email |> Pool_event.email_verification
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
