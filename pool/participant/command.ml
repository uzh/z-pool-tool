module Sign_up = struct
  type t =
    { email : string
    ; password : string
    ; firstname : string
    ; lastname : string
    ; recruitment_channel : string
    ; terms_accepted_at : Sihl.timestamp
    }

  type handle =
    ?allowed_email_suffixes:string list
    -> ?password_policy:(string -> (unit, string) Result.t)
    -> t
    -> (Event.t list, string) Result.t

  type can = Sihl.User.t -> bool Lwt.t
end

module Update = struct
  type t =
    { id : string
    ; firstname : string
    ; lastname : string
    ; email : string
    ; password : string
    ; paused : string
    }

  type handle_update_command =
    t -> Entity.participant -> (Event.t list, string) Result.t

  type can = Sihl.User.t -> bool Lwt.t
end
