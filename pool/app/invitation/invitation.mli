module ResentAt : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val create : unit -> t
  val value : t -> Ptime.t
end

module SendCount : sig
  include Pool_model.Base.IntegerSig

  val init : t
  val of_int : int -> t
  val increment : t -> t
end

type t =
  { id : Pool_common.Id.t
  ; contact : Contact.t
  ; resent_at : ResentAt.t option
  ; send_count : SendCount.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val create : ?id:Pool_common.Id.t -> Contact.t -> t

type notification_history =
  { invitation : t
  ; queue_entries : (Sihl_email.t * Queue.Instance.t) list
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
  | Resent of (t * Mailing.Id.t option)

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Database.Label.t -> event -> unit Lwt.t

val find
  :  Database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_message.Error.t) Lwt_result.t

val find_by_experiment
  :  ?query:Query.t
  -> Database.Label.t
  -> Experiment.Id.t
  -> (t list * Query.t) Lwt.t

val find_by_contact : Database.Label.t -> Contact.t -> t list Lwt.t

val find_experiment_id_of_invitation
  :  Database.Label.t
  -> t
  -> (Experiment.Id.t, Pool_message.Error.t) result Lwt.t

val find_multiple_by_experiment_and_contacts
  :  Database.Label.t
  -> Pool_common.Id.t list
  -> Experiment.t
  -> Pool_common.Id.t list Lwt.t

val find_by_contact_and_experiment_opt
  :  Database.Label.t
  -> Experiment.Id.t
  -> Contact.Id.t
  -> t option Lwt.t

val column_resent_at : Query.Column.t
val column_created_at : Query.Column.t
val column_count : Query.Column.t
val filterable_by : Query.Filter.human option
val searchable_by : Query.Column.t list
val sortable_by : Query.Column.t list
val default_query : Query.t

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Pool_message.Error.t) Lwt_result.t

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
