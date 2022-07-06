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

module PublicTitle : sig
  type t = string

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val value : t -> string
  val create : t -> (t, Pool_common.Message.error) result

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

module DirectRegistrationDisabled : sig
  type t

  val equal : t -> t -> t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : bool -> t
  val value : t -> bool

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module RegistrationDisabled : sig
  type t

  val equal : t -> t -> t
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : bool -> t
  val value : t -> bool

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

type t =
  { id : Id.t
  ; title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t
  ; filter : string
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

val create
  :  ?id:Id.t
  -> Title.t
  -> PublicTitle.t
  -> Description.t
  -> DirectRegistrationDisabled.t
  -> RegistrationDisabled.t
  -> t

type create =
  { title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  }

val equal_create : create -> create -> bool
val pp_create : Format.formatter -> create -> unit
val show_create : create -> string

module Public : sig
  type t =
    { id : Id.t
    ; public_title : PublicTitle.t
    ; description : Description.t
    ; direct_registration_disabled : DirectRegistrationDisabled.t
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

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

val find_public
  :  Pool_database.Label.t
  -> Id.t
  -> Contact.t
  -> (Public.t, Pool_common.Message.error) result Lwt.t

val find_all_public_by_contact
  :  Pool_database.Label.t
  -> Contact.t
  -> Public.t list Lwt.t

val session_count
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (int, Pool_common.Message.error) Lwt_result.t

val possible_participant_count : t -> int Lwt.t
val possible_participants : t -> Contact.t list Lwt.t
val title_value : t -> string
val public_title_value : t -> string
val description_value : t -> string
val direct_registration_disabled_value : t -> bool
val registration_disabled_value : t -> bool

module Repo : sig
  module Description : sig
    val t : Description.t Caqti_type.t
  end

  module DirectRegistrationDisabled : sig
    val t : bool Caqti_type.t
  end
end
