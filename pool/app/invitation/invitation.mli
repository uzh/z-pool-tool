module ResentAt : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val create : unit -> t
  val value : t -> Ptime.t
end

module ResendCount : sig
  include Pool_common.Model.IntegerSig

  val init : t
  val of_int : int -> t
end

type t =
  { id : Pool_common.Id.t
  ; contact : Contact.t
  ; resent_at : ResentAt.t option
  ; resend_count : ResendCount.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val create : ?id:Pool_common.Id.t -> Contact.t -> t

type notification_history =
  { invitation : t
  ; queue_entries : (Sihl_email.t * Sihl_queue.instance) list
  }

val equal_notification_history
  :  notification_history
  -> notification_history
  -> bool

val pp_notification_history : Format.formatter -> notification_history -> unit
val email_experiment_elements : Experiment.t -> (string * string) list

type create =
  { experiment : Experiment.t
  ; mailing : Mailing.t option
  ; contacts : Contact.t list
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type event =
  | Created of create
  | Resent of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) Lwt_result.t

val find_by_experiment
  :  ?query:Query.t
  -> Pool_database.Label.t
  -> Experiment.Id.t
  -> (t list * Query.t, Pool_common.Message.error) result Lwt.t

val find_by_contact
  :  Pool_database.Label.t
  -> Contact.t
  -> (t list, Pool_common.Message.error) result Lwt.t

val find_experiment_id_of_invitation
  :  Pool_database.Label.t
  -> t
  -> (Experiment.Id.t, Pool_common.Message.error) result Lwt.t

val find_multiple_by_experiment_and_contacts
  :  Pool_database.Label.t
  -> Pool_common.Id.t list
  -> Experiment.t
  -> Pool_common.Id.t list Lwt.t

val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Pool_common.Message.error) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index : Experiment.Id.t -> Guard.ValidationSet.t
    val create : Experiment.Id.t -> Guard.ValidationSet.t
    val read : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
    val update : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
    val delete : Experiment.Id.t -> Pool_common.Id.t -> Guard.ValidationSet.t
  end
end
