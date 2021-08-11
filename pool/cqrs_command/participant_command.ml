module Sign_up : sig
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
    ; recruitment_channel : Participant.RecruitmentChannel.t
    }

  val handle
    :  ?allowed_email_suffixes:string list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> t
    -> (Participant.event list, string) Result.t

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t
end = struct
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
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
            (fun l -> l |> List.hd |> Participant.recruitment_channel_of_string)
            (fun l -> [ Participant.recruitment_channel_to_string l ])
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
    let* () = Participant.validate_email allowed_email_suffixes command.email in
    let participant =
      Participant.
        { email = command.email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        ; recruitment_channel = command.recruitment_channel
        ; terms_accepted_at = Sihl.now ()
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
    :  Participant.participant Participant.t
    -> email:string
    -> password:string
    -> (Participant.event list, string) Result.t

  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; firstname : string
    ; lastname : string
    ; paused : string
    }

  let handle _ ~email:_ ~password:_ = Sihl.todo ()

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

  val handle
    :  t
    -> Participant.participant Participant.t
    -> (Participant.event list, string) Result.t

  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; current_password : string
    ; new_password : string
    }

  let handle _ = Sihl.todo

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

  val handle
    :  t
    -> Participant.participant Participant.t
    -> (Participant.event list, string) Result.t

  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; email : string
    }

  let handle _ = Sihl.todo

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
