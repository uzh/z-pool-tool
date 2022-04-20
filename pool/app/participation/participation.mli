module ShowUp : sig
  type t
end

module Participated : sig
  type t
end

module MatchesFilter : sig
  type t
end

module CanceledAt : sig
  type t

  val init : t
  val create_now : unit -> t
end

type t =
  { id : Pool_common.Id.t
  ; participant : Participant.t
  ; show_up : ShowUp.t
  ; participated : Participated.t
  ; matches_filter : MatchesFilter.t
  ; canceled_at : CanceledAt.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (Entity.t, Pool_common.Message.error) result Lwt.t
