module Create = struct
  type t =
    { start : Ptime.t
    ; duration : Ptime.Span.t
    ; description : Session.Description.t option
    ; max_participants : Session.ParticipantAmount.t
    ; min_participants : Session.ParticipantAmount.t
    }
end

module Update = struct
  type t =
    { session : Session.t
    ; start : Ptime.t
    ; duration : Ptime.Span.t
    ; description : Session.Description.t option
    ; max_participants : Session.ParticipantAmount.t
    ; min_participants : Session.ParticipantAmount.t
    }
end

module Delete = struct
  type t = Session.t

  (* Only when no participations added *)
end

module Cancel = struct
  (* TODO issue #90 step 2 *)
  (* notify_via: Email, SMS *)
  type t =
    { session : Session.t
    ; notify_via : string
    }
end

module ChangeLocation = struct
  (* TODO issue #90 step 2 *)
  (* notify_via: Email, SMS *)
  type t =
    { session : Session.t
    ; location : Location.t
    ; notify_via : string
    }
end

module ChangeSessionDate = struct
  (* TODO issue #90 step 3 *)
  (* notify_via: Email, SMS *)
  type t =
    { session : Session.t
    ; new_date : Ptime.t
    ; notify_via : string
    }
end
