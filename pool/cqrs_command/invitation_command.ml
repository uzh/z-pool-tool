module Create = struct
  type t =
    { experiment : Experiment.t
    ; participant : Participant.t
    }
  (* Adds invitation to mail queue *)
end

module Resend = struct
  type t = Invitation.t
end
