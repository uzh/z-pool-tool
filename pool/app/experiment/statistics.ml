module Field = Pool_message.Field
module Model = Pool_model.Base

module SentInvitations = struct
  type statistics =
    { invitation_resets : Entity.InvitationReset.t list
    ; sent_since_last_reset : int
    ; total_match_filter : int
    }
  [@@deriving eq, show]
end
