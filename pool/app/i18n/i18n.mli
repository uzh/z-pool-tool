module Key : sig
  type t =
    | AssistantRoleHint
    | CreditsText
    | ExperimenterRoleHint
    | GreetingsText
    | PasswordPolicyText
    | PrivacyPolicy
    | SignUpCTA
    | TermsAndConditions
    | WelcomeText

  val create : string -> (t, Pool_common.Message.error) result
  val show : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_rich_text : t -> bool
  val all : t list

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Content : sig
  include Pool_common.Model.StringSig
end

type t

val show : t -> string
val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val create : Key.t -> Pool_common.Language.t -> Content.t -> t

type create =
  { key : Key.t
  ; language : Pool_common.Language.t
  ; content : Content.t
  }

val id : t -> Pool_common.Id.t
val key : t -> Key.t
val language : t -> Pool_common.Language.t
val content : t -> Content.t
val content_to_string : t -> string

type event =
  | Created of create
  | Updated of t * Content.t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val find : Pool_database.Label.t -> Pool_common.Id.t -> t Lwt.t

val find_with_default_content
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> t Lwt.t

val find_by_key
  :  Pool_database.Label.t
  -> Key.t
  -> Pool_common.Language.t
  -> t Lwt.t

val find_by_key_opt
  :  Pool_database.Label.t
  -> Key.t
  -> Pool_common.Language.t
  -> t option Lwt.t

val find_all : Pool_database.Label.t -> unit -> t list Lwt.t
val terms_and_conditions_last_updated : Pool_database.Label.t -> Ptime.t Lwt.t

module I18nPageCache : sig
  val clear : unit -> unit
end

val i18n_is_set
  :  Pool_database.Label.t
  -> Pool_common.Language.t
  -> Key.t
  -> bool Lwt.t

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Guard.Target.t, Entity.PoolError.error) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val update : Pool_common.Id.t -> Guard.ValidationSet.t
  end
end
