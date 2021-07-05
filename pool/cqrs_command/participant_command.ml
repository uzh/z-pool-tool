module Sign_up : sig
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
    ; recruitment_channel : string
    ; terms_accepted_at : Sihl.timestamp
    }

  val handle
    :  ?allowed_email_suffixes:string list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> t
    -> (Participant.event list, string) Result.t
end = struct
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
    ; recruitment_channel : string
    ; terms_accepted_at : Sihl.timestamp
    }

  let handle ?allowed_email_suffixes:_ ?password_policy:_ _ = Sihl.todo
end

module UpdateDetails : sig
  type t =
    { id : string
    ; firstname : string
    ; lastname : string
    ; paused : string
    }

  val handle
    :  Participant.participant
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

  let handle _ ~email:_ ~password:_ = Sihl.todo

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
    -> Participant.participant
    -> (Participant.event list, string) Result.t

  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; current_password : string
    ; new_password : string
    }

  let handle = Sihl.todo

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
    -> Participant.participant
    -> (Participant.event list, string) Result.t

  val can : Sihl.User.t -> t -> bool Lwt.t
end = struct
  type t =
    { id : string
    ; email : string
    }

  let handle = Sihl.todo

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
