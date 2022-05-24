type t =
  { id : Pool_common.Id.t
  ; contact : Contact.t
  ; experiment : Experiment.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val create : ?id:Pool_common.Id.t -> Contact.t -> Experiment.t -> t

type create =
  { experiment : Experiment.Public.t
  ; contact : Contact.t
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type event =
  | Created of create
  | Deleted of create

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t

module ExperimentList : sig
  type waiting_list_entry =
    { id : Pool_common.Id.t
    ; contact : Contact.Preview.t
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
