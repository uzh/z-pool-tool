module Id = Pool_common.Id

module Location : sig
  module Room : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : string -> (t, string) result
  end

  module Building : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : string -> (t, string) result
  end

  module Street : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : string -> (t, string) result
  end

  module Zip : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : string -> (t, string) result
  end

  module City : sig
    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : string -> (t, string) result
  end

  type t =
    { id : Id.t
    ; room : Room.t
    ; building : Building.t
    ; street : Street.t
    ; zip : Zip.t
    ; city : City.t
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Title : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end

module Description : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : string -> (t, string) result
end

module ExperimentDate : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : Ptime.date -> (t, string) result
  val value : t -> Ptime.t
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string
val create : ?id:Id.t -> Title.t -> Description.t -> unit -> t

type create =
  { title : Title.t
  ; description : Description.t
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string
val equal_update : Event.update -> Event.update -> bool
val pp_update : Format.formatter -> Event.update -> unit
val show_update : Event.update -> string

type event =
  | ExperimentAdded of create
  | ExperimentEdited of t * Event.update
  | ExperimentDestroyed of t
  | ExperimenterAssigned of t * Admin__Entity.experimenter Admin__Entity.t
  | ExperimenterDivested of t * Admin__Entity.experimenter Admin__Entity.t
  | AssistantAssigned of t * Admin__Entity.assistant Admin__Entity.t
  | AssistantDivested of t * Admin__Entity.assistant Admin__Entity.t

val handle_event : Database_pool.Label.t -> event -> unit Lwt.t
val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit

type add = t -> t Lwt.t
type update = t -> t Lwt.t
type destroy = t -> t Lwt.t
