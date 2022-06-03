module Comment : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val value : t -> string
  val create : string -> t

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

type t =
  { id : Pool_common.Id.t
  ; contact : Contact.t
  ; experiment : Experiment.t
  ; comment : Comment.t option
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit

type create =
  { experiment : Experiment.Public.t
  ; contact : Contact.t
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type update = { comment : Comment.t option }

val equal_update : update -> update -> bool
val pp_update : Format.formatter -> update -> unit
val show_update : update -> string

val create
  :  ?id:Pool_common.Id.t
  -> Contact.t
  -> Experiment.t
  -> Comment.t option
  -> t

type event =
  | Created of create
  | Updated of update * t
  | Deleted of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

module ExperimentList : sig
  type waiting_list_entry =
    { id : Pool_common.Id.t
    ; contact : Contact.Preview.t
    ; comment : Comment.t option
    ; created_at : Ptime.t
    ; updated_at : Ptime.t
    }

  type t =
    { experiment : Experiment.t
    ; waiting_list_entries : waiting_list_entry list
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) Lwt_result.t

val user_is_enlisted
  :  Pool_database.Label.t
  -> Contact.t
  -> Experiment.Public.t
  -> bool Lwt.t

val find_by_experiment
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (ExperimentList.t, Pool_common.Message.error) Lwt_result.t

val find_by_contact_and_experiment
  :  Pool_database.Label.t
  -> Contact.t
  -> Experiment.Public.t
  -> (t option, Pool_common.Message.error) result Lwt.t
