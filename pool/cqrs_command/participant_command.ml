module Sign_up : sig
  type t =
    { email : Common.Email.Address.t
    ; password : Common.Password.t
    ; firstname : Common.Firstname.t
    ; lastname : Common.Lastname.t
    ; recruitment_channel : Participant.RecruitmentChannel.t
    }

  val handle
    :  ?allowed_email_suffixes:Settings.EmailSuffix.t list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> t
    -> (Participant.event list, string) Result.t

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t
end = struct
  type t =
    { email : Common.Email.Address.t
    ; password : Common.Password.t
    ; firstname : Common.Firstname.t
    ; lastname : Common.Lastname.t
    ; recruitment_channel : Participant.RecruitmentChannel.t
    }

  let command email password firstname lastname recruitment_channel =
    { email; password; firstname; lastname; recruitment_channel }
  ;;

  let default_password_policy p =
    if String.length p < 8 then Error "password_policy_text" else Ok ()
  ;;

  let schema =
    Conformist.(
      make
        [ string "email"
        ; string "password"
        ; string "firstname"
        ; string "lastname"
        ; custom
            (fun l -> l |> List.hd |> Participant.RecruitmentChannel.of_string)
            (fun l -> [ Participant.RecruitmentChannel.to_string l ])
            "recruitment_channel"
            ~meta:()
        ]
        command)
  ;;

  let handle
      ?allowed_email_suffixes
      ?(password_policy = default_password_policy)
      command
    =
    let ( let* ) = Result.bind in
    let* () = password_policy command.password in
    let* email =
      Participant.Email.Address.validate allowed_email_suffixes command.email
    in
    let participant =
      Participant.
        { email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        ; recruitment_channel = command.recruitment_channel
        ; terms_accepted_at = Ptime_clock.now ()
        }
    in
    Ok [ Participant.Created participant ]
  ;;

  let decode data = Conformist.decode_and_validate schema data
end

module UpdateDetails : sig
  type t =
    { id : string
    ; firstname : string
    ; lastname : string
    ; paused : string
    }

  val handle
    :  Participant.t
    -> email:string
    -> password:string
    -> (Participant.event list, string) Result.t

  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; firstname : string
    ; lastname : string
    ; paused : string
    }

  let handle _ ~email:_ ~password:_ = Utils.todo ()

  let can user command =
    let open Lwt.Syntax in
    let* participant = Participant.find_by_user user in
    let* tenant = Tenant.find_by_participant participant in
    Permission.can
      user
      ~any_of:
        [ Permission.Update (Permission.Participant, Some command.id)
        ; Permission.Update (Permission.Tenant, Some tenant.id)
        ]
  ;;
end

module UpdatePassword : sig
  type t =
    { id : string
    ; current_password : string
    ; new_password : string
    }

  val handle : t -> Participant.t -> (Participant.event list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; current_password : string
    ; new_password : string
    }

  let handle _ = Utils.todo

  let can user command =
    let open Lwt.Syntax in
    let* participant = Participant.find_by_user user in
    let* tenant = Tenant.find_by_participant participant in
    Permission.can
      participant.Participant.user
      ~any_of:
        [ Permission.Update (Permission.Participant, Some command.id)
        ; Permission.Update (Permission.Tenant, Some tenant.id)
        ]
  ;;
end

module UpdateEmail : sig
  type t =
    { id : string
    ; email : string
    }

  val handle : t -> Participant.t -> (Participant.event list, string) Result.t
  val can : Sihl_user.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; email : string
    }

  let handle _ = Utils.todo

  let can user command =
    let open Lwt.Syntax in
    let* participant = Participant.find_by_user user in
    let* tenant = Tenant.find_by_participant participant in
    Permission.can
      participant.Participant.user
      ~any_of:
        [ Permission.Update (Permission.Participant, Some command.id)
        ; Permission.Update (Permission.Tenant, Some tenant.id)
        ]
  ;;
end
