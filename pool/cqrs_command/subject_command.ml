module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user
module Id = Pool_common.Id

module SignUp : sig
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; recruitment_channel : Subject.RecruitmentChannel.t
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
    ; recruitment_channel : Subject.RecruitmentChannel.t
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
          ; Subject.RecruitmentChannel.schema ()
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
    let subject =
      Subject.
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
      [ Subject.Created subject |> Pool_event.subject
      ; Email.Created
          ( command.email
          , user_id
          , command.firstname
          , command.lastname
          , default_language |> CCOption.get_or ~default:Pool_common.Language.En
          )
        |> Pool_event.email_address
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module DeleteUnverified : sig
  val handle
    :  Subject.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  let handle subject =
    if subject.Subject.email_verified |> User.EmailVerified.is_some
    then Error Pool_common.Message.EmailDeleteAlreadyVerified
    else Ok [ Subject.UnverifiedDeleted subject |> Pool_event.subject ]
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
    :  Subject.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can
    :  Pool_tenant.Database.Label.t
    -> Sihl_user.t
    -> Subject.t
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

  let handle subject (command : t) =
    Subject.
      [ command.firstname |> CCOption.map (firstnameupdated subject)
      ; command.lastname |> CCOption.map (lastnameupdated subject)
      ; command.paused |> CCOption.map (pausedupdated subject)
      ; command.language |> CCOption.map (languageupdated subject)
      ]
    |> CCList.filter_map CCFun.id
    |> CCList.map Pool_event.subject
    |> CCResult.pure
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let can pool user subject =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update (Permission.Subject, Some (subject |> Subject.id))
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
    -> Subject.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can
    :  Pool_tenant.Database.Label.t
    -> Sihl_user.t
    -> Subject.t
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

  let handle ?password_policy subject command =
    let open CCResult in
    let* () =
      User.Password.validate_current_password
        subject.Subject.user
        command.current_password
    in
    let* () = User.Password.validate ?password_policy command.new_password in
    let* () =
      User.Password.validate_password_confirmation
        command.new_password
        command.password_confirmation
    in
    Ok
      [ Subject.PasswordUpdated
          ( subject
          , command.current_password
          , command.new_password
          , command.password_confirmation
          , subject.Subject.language
            |> CCOption.get_or ~default:Pool_common.Language.En )
        |> Pool_event.subject
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let can pool user subject =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update (Permission.Subject, Some (subject |> Subject.id))
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
    -> Subject.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can
    :  Pool_tenant.Database.Label.t
    -> Sihl_user.t
    -> Subject.t
    -> bool Lwt.t
end = struct
  type t = Pool_user.EmailAddress.t

  let handle ?allowed_email_suffixes subject email =
    let open CCResult in
    let* () = User.EmailAddress.validate allowed_email_suffixes email in
    Ok
      [ Email.Updated
          ( email
          , subject.Subject.user
          , subject.Subject.language
            |> CCOption.get_or ~default:Pool_common.Language.En )
        |> Pool_event.email_address
      ]
  ;;

  let can pool user subject =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update (Permission.Subject, Some (Subject.id subject))
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
    -> Subject.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Pool_database.Label.t -> Sihl_user.t -> Subject.t -> bool Lwt.t
end = struct
  type t = Email.unverified Email.t

  let handle ?allowed_email_suffixes subject email =
    let open CCResult in
    let* () =
      User.EmailAddress.validate allowed_email_suffixes (Email.address email)
    in
    Ok
      [ Subject.EmailUpdated (subject, Email.address email)
        |> Pool_event.subject
      ; Email.EmailVerified email |> Pool_event.email_address
      ]
  ;;

  let can pool user subject =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update (Permission.Subject, Some (Subject.id subject))
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
    :  Subject.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  let handle subject =
    Ok [ Subject.TermsAccepted subject |> Pool_event.subject ]
  ;;
end

module VerifyEmail : sig
  type t = { email : Email.unverified Email.t }

  val handle
    :  Subject.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = { email : Email.unverified Email.t }

  let handle subject command =
    Ok
      [ Subject.EmailVerified subject |> Pool_event.subject
      ; Email.EmailVerified command.email |> Pool_event.email_address
      ]
  ;;
end

module Verify = struct
  (* TODO issue #90 step 2 *)
  (* TODO Verify the subject itself with ID/Pass *)
end

module ToggleDisable = struct
  (* TODO issue #90 step 2 *)
  (* TODO Toggle disable command*)
end
