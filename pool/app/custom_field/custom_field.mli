module Answer : sig
  module Id : sig
    include Pool_common.Model.IdSig
  end

  type 'a t =
    { id : Id.t
    ; entity_uuid : Pool_common.Id.t
    ; value : 'a option
    ; admin_value : 'a option
    }

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string

  val create
    :  ?id:Id.t
    -> ?admin_value:'a
    -> Pool_common.Id.t
    -> 'a option
    -> 'a t

  val id : 'a t -> Id.t
  val value : 'a t -> 'a option
  val admin_value : 'a t -> 'a option
end

module Id : sig
  include Pool_common.Model.IdSig
end

module Model : sig
  type t = Contact

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val all : t list
  val create : string -> (t, Pool_common.Message.error) result

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t

  val to_nav_link : t -> Pool_common.I18n.nav_link
end

module Name : sig
  type name

  val equal_name : name -> name -> bool
  val pp_name : Format.formatter -> name -> unit
  val show_name : name -> string
  val name_of_yojson : Yojson.Safe.t -> name
  val yojson_of_name : name -> Yojson.Safe.t
  val value_name : name -> string

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> name
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val find_opt : Pool_common.Language.t -> t -> name option
  val find_opt_or : Pool_common.Language.t -> string -> t -> string

  val create
    :  Pool_common.Language.t list
    -> (Pool_common.Language.t * string) list
    -> (t, Pool_common.Message.error) result
end

module Hint : sig
  type hint

  val equal_hint : hint -> hint -> bool
  val pp_hint : Format.formatter -> hint -> unit
  val show_hint : hint -> string
  val hint_of_yojson : Yojson.Safe.t -> hint
  val yojson_of_hint : hint -> Yojson.Safe.t
  val value_hint : hint -> string

  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> hint
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val find_opt : Pool_common.Language.t -> t -> hint option

  val create
    :  (Pool_common.Language.t * string) list
    -> (t, Pool_common.Message.error) result
end

module FieldType : sig
  type t =
    | Boolean
    | Date
    | MultiSelect
    | Number
    | Select
    | Text

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val all : t list
  val to_string : t -> string

  val schema
    :  unit
    -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
end

module Required : sig
  include Pool_common.Model.BooleanSig
end

module Disabled : sig
  include Pool_common.Model.BooleanSig
end

module PublishedAt : sig
  include Pool_common.Model.PtimeSig
end

module AdminHint : sig
  include Pool_common.Model.StringSig
end

module AdminOverride : sig
  include Pool_common.Model.BooleanSig
end

module AdminViewOnly : sig
  include Pool_common.Model.BooleanSig
end

module AdminInputOnly : sig
  include Pool_common.Model.BooleanSig
end

module PromptOnRegistration : sig
  include Pool_common.Model.BooleanSig
end

module Validation : sig
  type raw = (string * string) list
  type 'a t = ('a -> ('a, Pool_common.Message.error) result) * raw

  module Number : sig
    type key =
      | NumberMin
      | NumberMax

    val show_key : key -> string

    val schema
      :  (string * string) list
      -> (int -> (int, Pool_common.Message.error) result) * raw
  end

  module Text : sig
    type key =
      | TextLengthMin
      | TextLengthMax

    val show_key : key -> string

    val schema
      :  (string * string) list
      -> (string -> (string, Pool_common.Message.error) result) * raw
  end

  module MultiSelect : sig
    type key =
      | OptionsCountMin
      | OptionsCountMax

    val show_key : key -> string

    val schema
      :  (string * string) list
      -> ('a list -> ('a list, Pool_common.Message.error) result) * raw
  end

  val key_to_human : string -> string
  val raw_of_yojson : Yojson.Safe.t -> raw
  val all : (string * [> `Number ] * FieldType.t) list
  val pure : 'a t
end

module SelectOption : sig
  module Id : sig
    include Pool_common.Model.IdSig
  end

  type t =
    { id : Id.t
    ; name : Name.t
    ; published_at : PublishedAt.t option
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val show_id : t -> string
  val name : Pool_common.Language.t -> t -> string
  val create : ?id:Id.t -> ?published_at:PublishedAt.t -> Name.t -> t

  val to_common_field
    :  Pool_common.Language.t
    -> t
    -> Pool_common.Message.Field.t

  module Public : sig
    type t =
      { id : Id.t
      ; name : Name.t
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val show_id : t -> string
    val name : Pool_common.Language.t -> t -> string
    val create : ?id:Id.t -> Name.t -> t

    val to_common_field
      :  Pool_common.Language.t
      -> t
      -> Pool_common.Message.Field.t
  end
end

module Public : sig
  type 'a public =
    { id : Id.t
    ; name : Name.t
    ; hint : Hint.t
    ; validation : 'a Validation.t
    ; required : Required.t
    ; admin_override : AdminOverride.t
    ; admin_input_only : AdminInputOnly.t
    ; prompt_on_registration : PromptOnRegistration.t
    ; version : Pool_common.Version.t
    }

  val equal_public : ('a -> 'a -> bool) -> 'a public -> 'a public -> bool

  val pp_public
    :  (Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a public
    -> unit

  val show_public : (Format.formatter -> 'a -> unit) -> 'a public -> string

  type t =
    | Boolean of bool public * bool Answer.t option
    | Date of Ptime.date public * Ptime.date Answer.t option
    | MultiSelect of
        SelectOption.Public.t list public
        * SelectOption.Public.t list
        * SelectOption.Public.t list Answer.t option
    | Number of int public * int Answer.t option
    | Select of
        SelectOption.Public.t public
        * SelectOption.Public.t list
        * SelectOption.Public.t Answer.t option
    | Text of string public * string Answer.t option

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val id : t -> Id.t
  val entity_id : t -> Pool_common.Id.t option
  val name_value : Pool_common.Language.t -> t -> string
  val hint : Pool_common.Language.t -> t -> Hint.hint option
  val required : t -> Required.t
  val admin_override : t -> AdminOverride.t
  val admin_input_only : t -> AdminInputOnly.t
  val prompt_on_registration : t -> PromptOnRegistration.t
  val is_disabled : bool -> t -> bool
  val version : t -> Pool_common.Version.t
  val field_type : t -> FieldType.t
  val increment_version : t -> t

  val to_common_field
    :  Pool_common.Language.t
    -> t
    -> Pool_common.Message.Field.t

  val to_common_hint
    :  Pool_common.Language.t
    -> t
    -> Pool_common.I18n.hint option

  val validation_hints : t -> Pool_common.I18n.hint list option
  val help_elements : Pool_common.Language.t -> t -> Pool_common.I18n.hint list
end

module Group : sig
  module Id : sig
    include Pool_common.Model.IdSig

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end

  type t =
    { id : Id.t
    ; model : Model.t
    ; name : Name.t
    }

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : ?id:Id.t -> Model.t -> Name.t -> t
  val name : Pool_common.Language.t -> t -> string

  module Public : sig
    type t =
      { id : Id.t
      ; name : Name.t
      ; fields : Public.t list
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val name : Pool_common.Language.t -> t -> string
  end
end

type 'a custom_field =
  { id : Id.t
  ; model : Model.t
  ; name : Name.t
  ; hint : Hint.t
  ; validation : 'a Validation.t
  ; required : Required.t
  ; disabled : Disabled.t
  ; custom_field_group_id : Group.Id.t option
  ; admin_hint : AdminHint.t option
  ; admin_override : AdminOverride.t
  ; admin_view_only : AdminViewOnly.t
  ; admin_input_only : AdminInputOnly.t
  ; prompt_on_registration : PromptOnRegistration.t
  ; published_at : PublishedAt.t option
  ; show_on_session_close_page : bool
  ; show_on_session_detail_page : bool
  }

type t =
  | Boolean of bool custom_field
  | Date of Ptime.date custom_field
  | Number of int custom_field
  | MultiSelect of SelectOption.t list custom_field * SelectOption.t list
  | Select of SelectOption.t custom_field * SelectOption.t list
  | Text of string custom_field

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
val show : t -> string

val create
  :  ?id:Id.t
  -> ?select_options:SelectOption.t list
  -> ?published_at:PublishedAt.t
  -> FieldType.t
  -> Model.t
  -> Name.t
  -> Hint.t
  -> (string * string) list
  -> Required.t
  -> Disabled.t
  -> Group.Id.t option
  -> AdminHint.t option
  -> AdminOverride.t
  -> AdminViewOnly.t
  -> AdminInputOnly.t
  -> PromptOnRegistration.t
  -> (t, Pool_common.Message.error) result

val boolean_fields : Pool_common.Message.Field.t list
val has_options : t -> (unit, Pool_common.Message.error) result
val id : t -> Id.t
val model : t -> Model.t
val name : t -> Name.t
val name_value : Pool_common.Language.t -> t -> string
val hint : t -> Hint.t
val required : t -> Required.t
val disabled : t -> Disabled.t
val published_at : t -> PublishedAt.t option
val group_id : t -> Group.Id.t option
val admin_hint : t -> AdminHint.t option
val admin_override : t -> AdminOverride.t
val admin_view_only : t -> AdminViewOnly.t
val admin_input_only : t -> AdminInputOnly.t
val prompt_on_registration : t -> PromptOnRegistration.t
val show_on_session_close_page : t -> bool
val set_show_on_session_close_page : bool -> t -> t
val show_on_session_detail_page : t -> bool
val set_show_on_session_detail_page : bool -> t -> t
val field_type : t -> FieldType.t
val validation_strings : t -> (string * string) list
val validation_to_yojson : t -> Yojson.Safe.t

module PartialUpdate : sig
  type t =
    | Firstname of Pool_common.Version.t * Pool_user.Firstname.t
    | Lastname of Pool_common.Version.t * Pool_user.Lastname.t
    | Language of Pool_common.Version.t * Pool_common.Language.t option
    | Custom of Public.t

  val is_required : t -> bool
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val increment_version : t -> t
end

val validate_htmx
  :  is_admin:bool
  -> entity_uuid:Pool_common.Id.t
  -> string list
  -> Public.t
  -> (Public.t, Pool_common.Message.error) result

val validate_partial_update
  :  ?is_admin:bool
  -> Contact.t
  -> Public.t option
  -> Pool_common.Message.Field.t * Pool_common.Version.t * string list
  -> (PartialUpdate.t, Pool_common.Message.error) Lwt_result.t

type event =
  | AdminAnswerCleared of Public.t * Pool_common.Id.t
  | AnswerUpserted of Public.t * Pool_common.Id.t * Pool_context.user
  | AnsweredOnSignup of Public.t * Pool_common.Id.t
  | Created of t
  | Deleted of t
  | FieldsSorted of t list
  | GroupCreated of Group.t
  | GroupDestroyed of Group.t
  | GroupsSorted of Group.t list
  | GroupUpdated of Group.t
  | OptionCreated of (Id.t * SelectOption.t)
  | OptionDestroyed of SelectOption.t
  | OptionPublished of SelectOption.t
  | OptionsSorted of SelectOption.t list
  | OptionUpdated of SelectOption.t
  | PartialUpdate of PartialUpdate.t * Contact.t * Pool_context.user
  | Published of t
  | Updated of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val find_by_model : Pool_database.Label.t -> Model.t -> t list Lwt.t
val find_by_group : Pool_database.Label.t -> Group.Id.t -> t list Lwt.t
val find_ungrouped_by_model : Pool_database.Label.t -> Model.t -> t list Lwt.t

val find
  :  Pool_database.Label.t
  -> Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_by_table_view
  :  Pool_database.Label.t
  -> [< `SessionClose | `SessionDetail ]
  -> t list Lwt.t

val find_all_by_contact
  :  Pool_database.Label.t
  -> Pool_context.user
  -> Pool_common.Id.t
  -> (Group.Public.t list * Public.t list) Lwt.t

val find_all_required_by_contact
  :  Pool_database.Label.t
  -> Pool_context.user
  -> Pool_common.Id.t
  -> (Group.Public.t list * Public.t list) Lwt.t

val find_unanswered_required_by_contact
  :  Pool_database.Label.t
  -> Pool_context.user
  -> Pool_common.Id.t
  -> (Group.Public.t list * Public.t list) Lwt.t

val find_unanswered_ungrouped_required_by_contact
  :  Pool_database.Label.t
  -> Pool_context.user
  -> Pool_common.Id.t
  -> Public.t list Lwt.t

val find_multiple_by_contact
  :  ?is_admin:bool
  -> Pool_database.Label.t
  -> Pool_common.Id.t
  -> Pool_common.Id.t list
  -> Public.t list Lwt.t

val find_by_contact
  :  ?is_admin:bool
  -> Pool_database.Label.t
  -> Pool_common.Id.t
  -> Id.t
  -> (Public.t, Pool_common.Message.error) result Lwt.t

val all_required_answered
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> bool Lwt.t

val all_answered : Pool_database.Label.t -> Pool_common.Id.t -> bool Lwt.t
val all_prompted_on_registration : Pool_database.Label.t -> Public.t list Lwt.t

val find_public_by_contacts_and_view
  :  Pool_database.Label.t
  -> bool
  -> Contact.Id.t list
  -> [< `SessionClose | `SessionDetail ]
  -> Public.t list Lwt.t

val find_option
  :  Pool_database.Label.t
  -> SelectOption.Id.t
  -> (SelectOption.t, Pool_common.Message.error) result Lwt.t

val find_options_by_field
  :  Pool_database.Label.t
  -> Id.t
  -> SelectOption.t list Lwt.t

val find_group
  :  Pool_database.Label.t
  -> Group.Id.t
  -> (Group.t, Pool_common.Message.error) result Lwt.t

val find_groups_by_model
  :  Pool_database.Label.t
  -> Model.t
  -> Group.t list Lwt.t

module Repo : sig
  module Id : sig
    type t = Id.t

    val t : t Caqti_type.t
  end

  module SelectOption : sig
    module Id : sig
      type t = SelectOption.Id.t

      val t : t Caqti_type.t
    end
  end
end

val group_fields : Group.t list -> t list -> (Group.t * t list) list * t list

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

  type group_id = Group.Id.t

  module Group : sig
    module Target : sig
      val to_authorizable
        :  ?ctx:(string * string) list
        -> Group.t
        -> (Guard.Target.t, Pool_common.Message.error) Lwt_result.t

      type t

      val equal : t -> t -> bool
      val pp : Format.formatter -> t -> unit
      val show : t -> string
    end
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
    val delete : Id.t -> Guard.ValidationSet.t

    module Group : sig
      val index : Guard.ValidationSet.t
      val create : Guard.ValidationSet.t
      val update : group_id -> Guard.ValidationSet.t
      val delete : group_id -> Guard.ValidationSet.t
    end
  end
end
