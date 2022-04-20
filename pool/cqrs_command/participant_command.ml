module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user
module Id = Pool_common.Id

module SignUp : sig
  type t =
    { email : User.EmailAddress.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; recruitment_channel : Participant.RecruitmentChannel.t
    }

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:(string -> (unit, string) result)
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
    ; recruitment_channel : Participant.RecruitmentChannel.t
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
          ; Participant.RecruitmentChannel.schema ()
          ]
        command)
  ;;

  let handle ?allowed_email_suffixes ?password_policy default_language command =
    let open CCResult in
    let* () = User.Password.validate ?password_policy command.password in
    let* () = User.EmailAddress.validate allowed_email_suffixes command.email in
    let user_id = Id.create () in
    let participant =
      Participant.
        { user_id
        ; email = command.email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        ; recruitment_channel = command.recruitment_channel
        ; terms_accepted_at = User.TermsAccepted.create_now ()
        ; language = default_language
        }
    in
    Ok
      [ Participant.Created participant |> Pool_event.participant
      ; Email.Created
          (command.email, user_id, command.firstname, command.lastname)
        |> Pool_event.email_address
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_coformist_error
  ;;
end

module DeleteUnverified : sig
  val handle
    :  Participant.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  let handle participant =
    Ok
      [ Participant.UnverifiedDeleted (Participant.id participant)
        |> Pool_event.participant
      ]
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
    :  Participant.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can
    :  Pool_tenant.Database.Label.t
    -> Sihl_user.t
    -> Participant.t
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

  let handle participant (command : t) =
    Participant.
      [ command.firstname |> CCOption.map (firstnameupdated participant)
      ; command.lastname |> CCOption.map (lastnameupdated participant)
      ; command.paused |> CCOption.map (pausedupdated participant)
      ; command.language |> CCOption.map (languageupdated participant)
      ]
    |> CCList.filter_map CCFun.id
    |> CCList.map Pool_event.participant
    |> CCResult.pure
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_coformist_error
  ;;

  let can pool user participant =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update
              (Permission.Participant, Some (participant |> Participant.id))
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
    :  ?password_policy:(string -> (unit, string) result)
    -> Participant.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can
    :  Pool_tenant.Database.Label.t
    -> Sihl_user.t
    -> Participant.t
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

  let handle ?password_policy participant command =
    let open CCResult in
    let* () = User.Password.validate ?password_policy command.new_password in
    Ok
      [ Participant.PasswordUpdated
          ( participant
          , command.current_password
          , command.new_password
          , command.password_confirmation )
        |> Pool_event.participant
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_coformist_error
  ;;

  let can pool user participant =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update
              (Permission.Participant, Some (participant |> Participant.id))
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
    -> Participant.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can
    :  Pool_tenant.Database.Label.t
    -> Sihl_user.t
    -> Participant.t
    -> bool Lwt.t
end = struct
  type t = Pool_user.EmailAddress.t

  let handle ?allowed_email_suffixes participant email =
    let open CCResult in
    let* () = User.EmailAddress.validate allowed_email_suffixes email in
    Ok
      [ Email.Updated (email, participant.Participant.user)
        |> Pool_event.email_address
      ]
  ;;

  let can pool user participant =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update
              (Permission.Participant, Some (Participant.id participant))
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
    -> Participant.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Pool_database.Label.t -> Sihl_user.t -> Participant.t -> bool Lwt.t
end = struct
  type t = Email.unverified Email.t

  let handle ?allowed_email_suffixes participant email =
    let open CCResult in
    let* () =
      User.EmailAddress.validate allowed_email_suffixes (Email.address email)
    in
    Ok
      [ Participant.EmailUpdated (participant, Email.address email)
        |> Pool_event.participant
      ; Email.EmailVerified email |> Pool_event.email_address
      ]
  ;;

  let can pool user participant =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update
              (Permission.Participant, Some (Participant.id participant))
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
    :  Participant.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  let handle participant =
    Ok [ Participant.TermsAccepted participant |> Pool_event.participant ]
  ;;
end

module VerifyEmail : sig
  type t = { email : Email.unverified Email.t }

  val handle
    :  t
    -> Participant.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type t = { email : Email.unverified Email.t }

  let handle command participant =
    (* TODO use email_verified here*)
    Ok
      [ Participant.AccountVerified participant |> Pool_event.participant
      ; Email.EmailVerified command.email |> Pool_event.email_address
      ]
  ;;
end

module Verify = struct
  (* TODO Verify the participant itself with ID/Pass *)
end

module ToggleDisable = struct
  (* TODO Toggle disable command*)
end
