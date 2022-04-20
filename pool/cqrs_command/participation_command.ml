module Create = struct
  type t =
    { participant : Participant.t
    ; session : Session.t
    }
end

module Cancel = struct
  type t = Pool_common.Id.t
end

module SetAttendance = struct
  type t =
    { participation : Participation.t
    ; show_up : bool
    ; participated : bool
    }
end
