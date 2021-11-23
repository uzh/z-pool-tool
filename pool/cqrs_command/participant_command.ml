module User = Common_user
module Id = Pool_common.Id

module SignUp : sig
  type t =
    { email : User.Email.Address.t
    ; password : User.Password.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; recruitment_channel : Participant.RecruitmentChannel.t
    }

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:(string -> (unit, string) result)
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result
end = struct
  type t =
    { email : User.Email.Address.t
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
          [ User.Email.Address.schema ()
          ; User.Password.schema ()
          ; User.Firstname.schema ()
          ; User.Lastname.schema ()
          ; Participant.RecruitmentChannel.schema ()
          ]
        command)
  ;;

  let handle ?allowed_email_suffixes ?password_policy command =
    let open CCResult in
    let* () = User.Password.validate ?password_policy command.password in
    let* () =
      Common_user.Email.Address.validate allowed_email_suffixes command.email
    in
    let participant =
      Participant.
        { email = command.email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        ; recruitment_channel = command.recruitment_channel
        ; terms_accepted_at = User.TermsAccepted.create_now ()
        }
    in
    Ok
      [ Participant.Created participant |> Pool_event.participant
      ; User.Event.Email.Created
          (command.email, command.firstname, command.lastname)
        |> Pool_event.email_address
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.conformist
  ;;
end

module UpdateDetails : sig
  type t =
    { firstname : User.Firstname.t option
    ; lastname : User.Lastname.t option
    ; paused : User.Paused.t option
    }

  val handle
    :  Participant.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val can
    :  Pool_common.Database.Label.t
    -> Sihl_user.t
    -> Participant.t
    -> bool Lwt.t
end = struct
  type t =
    { firstname : User.Firstname.t option
    ; lastname : User.Lastname.t option
    ; paused : User.Paused.t option
    }

  let command firstname lastname paused = { firstname; lastname; paused }

  let schema =
    Conformist.(
      make
        Field.
          [ Conformist.optional @@ User.Firstname.schema ()
          ; Conformist.optional @@ User.Lastname.schema ()
          ; Conformist.optional @@ User.Paused.schema ()
          ]
        command)
  ;;

  let handle participant (command : t) =
    Participant.
      [ command.firstname |> CCOption.map (firstnameupdated participant)
      ; command.lastname |> CCOption.map (lastnameupdated participant)
      ; command.paused |> CCOption.map (pausedupdated participant)
      ]
    |> CCList.filter_map CCFun.id
    |> CCList.map Pool_event.participant
    |> CCResult.pure
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.conformist
  ;;

  let can pool user participant =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        user
        ~any_of:
          [ Permission.Update
              (Permission.Participant, Some (participant |> Participant.id))
          ; Permission.Update (Permission.Tenant, Some tenant.Tenant.id)
          ]
    in
    pool
    |> Tenant.find_by_label
    |>> check_permission
    |> Lwt.map (CCResult.get_or ~default:false)
  ;;
end

module UpdatePassword : sig
  type t =
    { participant : Participant.t
    ; current_password : User.Password.t
    ; new_password : User.Password.t
    ; password_confirmation : User.PasswordConfirmed.t
    }

  val handle : t -> (Pool_event.t list, Pool_common.Message.error) result
  val can : Pool_common.Database.Label.t -> Participant.t -> t -> bool Lwt.t
end = struct
  type t =
    { participant : Participant.t
    ; current_password : User.Password.t
    ; new_password : User.Password.t
    ; password_confirmation : User.PasswordConfirmed.t
    }

  let handle command =
    Ok
      [ Participant.PasswordUpdated
          ( command.participant
          , command.current_password
          , command.new_password
          , command.password_confirmation )
        |> Pool_event.participant
      ]
  ;;

  let can pool participant command =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        participant.Participant.user
        ~any_of:
          [ Permission.Update
              ( Permission.Participant
              , Some (command.participant |> Participant.id) )
          ; Permission.Update (Permission.Tenant, Some tenant.Tenant.id)
          ]
    in
    pool
    |> Tenant.find_by_label
    |>> check_permission
    |> Lwt.map (CCResult.get_or ~default:false)
  ;;
end

module UpdateEmail : sig
  type t =
    { id : Id.t
    ; email : User.Email.Address.t
    }

  val handle
    :  t
    -> Participant.t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val can : Pool_common.Database.Label.t -> Participant.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : Id.t
    ; email : User.Email.Address.t
    }

  let handle _ = Utils.todo

  let can pool participant command =
    let open Utils.Lwt_result.Infix in
    let check_permission tenant =
      Permission.can
        participant.Participant.user
        ~any_of:
          [ Permission.Update (Permission.Participant, Some command.id)
          ; Permission.Update (Permission.Tenant, Some tenant.Tenant.id)
          ]
    in
    pool
    |> Tenant.find_by_label
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
    Ok [ Participant.AcceptTerms participant |> Pool_event.participant ]
  ;;
end

module ConfirmEmail : sig
  type t = { email : Common_user.Email.unverified Common_user.Email.t }

  val handle
    :  t
    -> Participant.t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  module Email = Common_user.Email

  type t = { email : Email.unverified Email.t }

  let handle command participant =
    Ok
      [ Participant.EmailConfirmed participant |> Pool_event.participant
      ; Common_user.Event.Email.Verified command.email
        |> Pool_event.email_address
      ]
  ;;
end
