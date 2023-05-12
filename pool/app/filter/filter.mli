module Id = Pool_common.Id

module Title : sig
  include Pool_common.Model.StringSig
end

type single_val =
  | Bool of bool
  | Date of Ptime.t
  | Language of Pool_common.Language.t
  | Nr of float
  | Option of Custom_field.SelectOption.Id.t
  | Str of string

type value =
  | NoValue
  | Single of single_val
  | Lst of single_val list

module Key : sig
  type input_type =
    | Bool
    | Date
    | Languages of Pool_common.Language.t list
    | Nr
    | Str
    | Select of Custom_field.SelectOption.t list
    | MultiSelect of Custom_field.SelectOption.t list
    | QueryExperiments

  val show_input_type : input_type -> string

  type hardcoded =
    | ContactLanguage
    | Firstname
    | Name
    | NumAssignments
    | NumInvitations
    | NumNoShows
    | NumParticipations
    | NumShowUps
    | Participation

  type t =
    | CustomField of Custom_field.Id.t
    | Hardcoded of hardcoded

  type human =
    | CustomField of Custom_field.t
    | Hardcoded of hardcoded

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
  val show : t -> string

  val hardcoded_to_single_value_sql
    :  hardcoded
    -> (string, Pool_common.Message.error) result

  val equal_human : human -> human -> bool
  val show_human : human -> string
  val type_of_key : human -> input_type
  val human_to_label : Pool_common.Language.t -> human -> string
  val human_to_value : human -> string
end

module Operator : sig
  module Equality : sig
    type t =
      | Equal
      | NotEqual
  end

  module Existence : sig
    type t =
      | Empty
      | NotEmpty
  end

  module ListM : sig
    type t =
      | ContainsSome
      | ContainsNone
      | ContainsAll
  end

  module Size : sig
    type t =
      | Less
      | LessEqual
      | Greater
      | GreaterEqual
  end

  module StringM : sig
    type t = Like
  end

  type t =
    | Equality of Equality.t
    | Existence of Existence.t
    | List of ListM.t
    | Size of Size.t
    | String of StringM.t

  val equality : Equality.t -> t
  val existence : Existence.t -> t
  val list : ListM.t -> t
  val size : Size.t -> t
  val string : StringM.t -> t
  val to_sql : t -> string
  val to_human : t -> string
  val equal : t -> t -> bool
  val show : t -> string
  val input_type_to_operator : Key.input_type -> t list
end

module Predicate : sig
  type t =
    { key : Key.t
    ; operator : Operator.t
    ; value : value
    }

  type human =
    { key : Key.human option
    ; operator : Operator.t option
    ; value : value option
    }

  val create : Key.t -> Operator.t -> value -> t
end

type query =
  | And of query list
  | Or of query list
  | Not of query
  | Pred of Predicate.t
  | Template of Pool_common.Id.t

val show_query : query -> string

type t =
  { id : Pool_common.Id.t
  ; query : query
  ; title : Title.t option
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }

module Human : sig
  type t =
    | And of t list
    | Or of t list
    | Not of t
    | Pred of Predicate.human
    | Template of Pool_common.Id.t option

  val show : t -> string
  val init : ?key:Key.human -> ?operator:Operator.t -> ?value:value -> unit -> t

  val of_yojson
    :  Key.human list
    -> Yojson.Safe.t
    -> (t, Pool_common.Message.error) result

  val all_query_experiments : t -> Pool_common.Id.t list
end

val equal : t -> t -> bool
val show : t -> string
val pp : Format.formatter -> t -> unit
val create : ?id:Pool_common.Id.t -> Title.t option -> query -> t
val yojson_of_query : query -> Yojson.Safe.t
val query_of_yojson : Yojson.Safe.t -> (query, Pool_common.Message.error) result
val query_of_string : string -> (query, Pool_common.Message.error) result

val validate_query
  :  Key.human list
  -> t list
  -> query
  -> (query, Pool_common.Message.error) result

val contains_template : query -> bool

val find
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_all_templates : Pool_database.Label.t -> unit -> t list Lwt.t

val find_template
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> (t, Pool_common.Message.error) result Lwt.t

val find_multiple_templates
  :  Pool_database.Label.t
  -> Pool_common.Id.t list
  -> t list Lwt.t

type event =
  | Created of t
  | Deleted of t
  | Updated of t

val equal_event : event -> event -> bool
val pp_event : Format.formatter -> event -> unit
val show_event : event -> string
val handle_event : Pool_database.Label.t -> event -> unit Lwt.t
val created : t -> event
val deleted : t -> event
val updated : t -> event

module UtilsF : sig
  type filter_label =
    | And
    | Or
    | Not
    | Pred
    | Template

  val equal_filter_label : filter_label -> filter_label -> bool
  val show_filter_label : filter_label -> string
  val to_label : filter_label -> string

  val label_of_string
    :  string
    -> (filter_label, Pool_common__Entity_message.error) result

  val all_filter_labels : filter_label list
  val default_filter_label : filter_label
end

module Repo : sig
  val t : t Caqti_type.t
end

val all_keys : Pool_database.Label.t -> Key.human list Lwt.t

val key_of_string
  :  Pool_database.Label.t
  -> string
  -> (Key.human, Pool_common.Message.error) Lwt_result.t

val t_to_human : Key.human list -> t list -> query -> Human.t
val find_templates_of_query : Pool_database.Label.t -> query -> t list Lwt.t

val toggle_predicate_type
  :  Human.t
  -> string
  -> (Human.t, Pool_common.Message.error) result

val all_query_experiments : t -> Pool_common.Id.t list

val find_filtered_contacts
  :  Pool_database.Label.t
  -> ?order_by:string
  -> ?limit:int
  -> Pool_common.Id.t
  -> t option
  -> (Contact.t list, Pool_common.Message.error) Lwt_result.t

val count_filtered_contacts
  :  Pool_database.Label.t
  -> Pool_common.Id.t
  -> query option
  -> (int, Pool_common.Message.error) Lwt_result.t

val contact_matches_filter
  :  ?default:bool
  -> Pool_database.Label.t
  -> Pool_common.Id.t
  -> t option
  -> Contact.t
  -> bool Lwt.t

module Guard : sig
  module Target : sig
    val to_authorizable
      :  ?ctx:(string * string) list
      -> t
      -> (Role.Target.t Guard.Target.t, Pool_common.Message.error) Lwt_result.t

    type t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
  end

  module Access : sig
    val index : Guard.ValidationSet.t
    val create : Guard.ValidationSet.t
    val update : Id.t -> Guard.ValidationSet.t
    val delete : Id.t -> Guard.ValidationSet.t
  end
end
