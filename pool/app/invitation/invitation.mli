module ResentAt : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val create : unit -> t
  val value : t -> Ptime.t
end

type t =
  { id : Pool_common.Id.t
  ; participant : Participant.t
  ; resent_at : ResentAt.t option
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val create : ?id:Pool_common.Id.t -> Participant.t -> t

type notification_history =
  { invitation : t
  ; queue_entries : (Sihl_email.t * Sihl_queue.instance) list
  }

val equal_notification_history
  :  notification_history
  -> notification_history
  -> bool

val pp_notification_history : Format.formatter -> notification_history -> unit

type create =
  { experiment : Experiment.t
  ; participant : Participant.t
  }

type resent =
  { invitation : t
  ; experiment : Experiment.t
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type event =
  | Created of create
  | Resent of resent

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) Lwt_result.t

val find_by_experiment
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t list, Pool_common.Message.error) result Lwt.t

val find_by_participant
  :  Pool_database.Label.t
  -> Participant.t
  -> (t list, Pool_common.Message.error) result Lwt.t

val find_experiment_id_of_invitation
  :  Pool_database.Label.t
  -> t
  -> (Pool_common.Id.t, Pool_common.Message.error) result Lwt.t
