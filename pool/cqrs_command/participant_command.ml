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
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> t
    -> (Pool_event.t list, string) Result.t

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t
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
        ; terms_accepted_at = User.TermsAccepted.create_now
        }
    in
    Ok
      [ Participant.Created participant |> Pool_event.participant
      ; User.Event.Email.Created
          (command.email, command.firstname, command.lastname)
        |> Pool_event.email_address
      ]
  ;;

  let decode data = Conformist.decode_and_validate schema data
end

module UpdateDetails : sig
  type t =
    { id : Id.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; paused : User.Paused.t
    }

  val handle
    :  Participant.t
    -> email:User.Email.Address.t
    -> password:User.Password.t
    -> (Pool_event.t list, string) Result.t

  val can : Pool_common.Database.Label.t -> Participant.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : Id.t
    ; firstname : User.Firstname.t
    ; lastname : User.Lastname.t
    ; paused : User.Paused.t
    }

  let handle _ ~email:_ ~password:_ = Utils.todo ()

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

module UpdatePassword : sig
  type t =
    { id : Id.t
    ; current_password : User.Password.t
    ; new_password : User.Password.t
    }

  val handle : t -> Participant.t -> (Pool_event.t list, string) Result.t
  val can : Pool_common.Database.Label.t -> Participant.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : Id.t
    ; current_password : User.Password.t
    ; new_password : User.Password.t
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

module UpdateEmail : sig
  type t =
    { id : Id.t
    ; email : User.Email.Address.t
    }

  val handle : t -> Participant.t -> (Pool_event.t list, string) Result.t
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
  val handle : Participant.t -> (Pool_event.t list, string) Result.t
end = struct
  let handle participant =
    Ok [ Participant.AcceptTerms participant |> Pool_event.participant ]
  ;;
end

module ConfirmEmail : sig
  type t = { email : Common_user.Email.unverified Common_user.Email.t }

  val handle : t -> Participant.t -> (Pool_event.t list, string) Result.t
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
