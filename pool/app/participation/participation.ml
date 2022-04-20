type t =
  { id : Pool_common.Id.t
  ; participant : Participant.t
  ; show_up : bool
  ; participated : bool
  ; matches_filter : bool
  ; canceled_at : Ptime.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]
