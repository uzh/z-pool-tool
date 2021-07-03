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
end

module Update = struct
  type t =
    { id : string
    ; firstname : string
    ; lastname : string
    ; paused : string
    }

  type handle = t -> Entity.participant -> (Event.t list, string) Result.t
  type can = Sihl.User.t -> bool Lwt.t
end

module UpdatePassword = struct
  type t =
    { id : string
    ; current_password : string
    ; password : string
    }

  type handle = t -> Entity.participant -> (Event.t list, string) Result.t
  type can = Sihl.User.t -> bool Lwt.t
end

module UpdateEmail = struct
  type t =
    { id : string
    ; email : string
    }

  type handle = t -> Entity.participant -> (Event.t list, string) Result.t
  type can = Sihl.User.t -> bool Lwt.t
end
