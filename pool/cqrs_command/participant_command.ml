module Sign_up : sig
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
    ; recruitment_channel : Person.RecruitmentChannel.t
    }

  val handle
    :  ?allowed_email_suffixes:string list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> t
    -> (Person.event list, string) Result.t

  val decode
    :  (string * string list) list
    -> (t, Conformist.error list) Result.t
end = struct
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
    ; recruitment_channel : Person.RecruitmentChannel.t
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
            (fun l -> l |> List.hd |> Person.recruitment_channel_of_string)
            (fun l -> [ Person.recruitment_channel_to_string l ])
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
    let* () = Person.validate_email allowed_email_suffixes command.email in
    let participant =
      Person.
        { email = command.email
        ; password = command.password
        ; firstname = command.firstname
        ; lastname = command.lastname
        ; recruitment_channel = command.recruitment_channel
        ; terms_accepted_at = Sihl.now ()
        }
    in
    Ok [ Person.Created participant ]
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
    :  Person.participant Person.t
    -> email:string
    -> password:string
    -> (Person.event list, string) Result.t

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
    let* participant = Person.find_by_user user in
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
    -> Person.participant Person.t
    -> (Person.event list, string) Result.t

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
    let* participant = Person.find_by_user user in
    let* tenant = Tenant.find_by_participant participant in
    Permission.can
      participant.Person.user
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
    -> Person.participant Person.t
    -> (Person.event list, string) Result.t

  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; email : string
    }

  let handle _ = Sihl.todo

  let can user command =
    let open Lwt.Syntax in
    let* participant = Person.find_by_user user in
    let* tenant = Tenant.find_by_participant participant in
    Permission.can
      participant.Person.user
      ~any_of:
        [ Permission.Update (Permission.Participant, Some command.id)
        ; Permission.Update (Permission.Tenant, Some tenant.id)
        ]
  ;;
end
