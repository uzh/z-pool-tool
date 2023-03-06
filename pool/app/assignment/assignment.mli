module Id : sig
  include module type of Pool_common.Id

  val to_common : t -> Pool_common.Id.t
end

module ShowUp : sig
  include Pool_common.Model.BooleanSig

  val init : t
end

module Participated : sig
  include Pool_common.Model.BooleanSig

  val init : t
end

module MatchesFilter : sig
  type t

  val init : t
end

module CanceledAt : sig
  include Pool_common.Model.PtimeSig
end

module MarkedAsDeleted : sig
  include Pool_common.Model.BooleanSig

  val init : t
end

type t =
  { id : Pool_common.Id.t
  ; contact : Contact.t
  ; show_up : ShowUp.t option
  ; participated : Participated.t option
  ; matches_filter : MatchesFilter.t
  ; canceled_at : CanceledAt.t option
  ; marked_as_deleted : MarkedAsDeleted.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool

val create
  :  ?id:Pool_common.Id.t
  -> ?show_up:ShowUp.t
  -> ?participated:Participated.t
  -> ?matches_filter:MatchesFilter.t
  -> ?canceled_at:CanceledAt.t
  -> ?marked_as_deleted:MarkedAsDeleted.t
  -> Contact.t
  -> t

val is_deletable : t -> (unit, Pool_common.Message.error) result
val is_cancellable : t -> (unit, Pool_common.Message.error) result
val attendance_settable : t -> (unit, Pool_common.Message.error) result

module Public : sig
  type t =
    { id : Pool_common.Id.t
    ; canceled_at : CanceledAt.t option
    }
end

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_by_experiment_and_contact_opt
  :  Pool_database.Label.t
  -> Experiment.Id.t
  -> Contact.t
  -> Public.t list Lwt.t

val find_by_session
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t list, Pool_common.Message.error) result Lwt.t

val find_uncanceled_by_session
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t list, Pool_common.Message.error) result Lwt.t

val find_deleted_by_session
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t list, Pool_common.Message.error) result Lwt.t

val find_with_follow_ups
  :  Pool_database.Label.t
  -> Id.t
  -> (t list, Pool_common.Message.error) result Lwt.t

type create =
  { contact : Contact.t
  ; session_id : Pool_common.Id.t
  }

type event =
  | AttendanceSet of (t * ShowUp.t * Participated.t)
  | Canceled of t
  | Created of create
  | MarkedAsDeleted of t

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
