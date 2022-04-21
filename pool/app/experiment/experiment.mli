module Id = Pool_common.Id

module Title : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val value : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Description : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, Pool_common.Message.error) result
  val value : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t
  ; filter : string
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val create : ?id:Id.t -> Title.t -> Description.t -> t

type create =
  { title : Title.t
  ; description : Description.t
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

type event =
  | Created of create
  | Updated of t * create
  | Destroyed of Pool_common.Id.t
  | ExperimenterAssigned of t * Admin__Entity.experimenter Admin__Entity.t
  | ExperimenterDivested of t * Admin__Entity.experimenter Admin__Entity.t
  | AssistantAssigned of t * Admin__Entity.assistant Admin__Entity.t
  | AssistantDivested of t * Admin__Entity.assistant Admin__Entity.t

val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Repo_entity.Common.Message.error) result Lwt.t

val find_all : Pool_database.Label.t -> unit -> t list Lwt.t

val session_count
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (int, Pool_common.Message.error) Lwt_result.t

val possible_participant_count : t -> int Lwt.t
val possible_participants : t -> Participant.t list Lwt.t
val title_value : t -> string
val description_value : t -> string
