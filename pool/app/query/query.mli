module Column : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val field : t -> Pool_message.Field.t
  val create : Pool_message.Field.t * string -> t
  val create_list : (Pool_message.Field.t * string) list -> t list
end

module Pagination : sig
  module Limit : Pool_model.Base.IntegerSig

  module Page : sig
    include Pool_model.Base.IntegerSig

    val default : t
  end

  module PageCount : Pool_model.Base.IntegerSig

  type t =
    { limit : Limit.t
    ; page : Page.t
    ; page_count : PageCount.t
    }

  val to_query_parts : t -> (Pool_message.Field.t * string) list
end

module Search : sig
  module Query : Pool_model.Base.StringSig

  type t =
    { query : Query.t
    ; columns : Column.t list
    }

  val query_string : t -> string
  val to_query_parts : t -> (Pool_message.Field.t * string) list
end

module Sort : sig
  module SortOrder : sig
    type t =
      | Ascending
      | Descending

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val to_human : Pool_common.Language.t -> t -> string
    val read : string -> t
    val create : string -> (t, Pool_message.Error.t) result
    val all : t list
    val default : t
    val flip : t -> t
    val schema : unit -> (Pool_message.Error.t, t) Pool_conformist.Field.t
  end

  type t =
    { column : Column.t
    ; order : SortOrder.t
    }

  val to_query_parts : t -> (Pool_message.Field.t * string) list
end

module Filter : sig
  module SelectOption : sig
    type label = (Pool_common.Language.t * string) list

    val equal_label : label -> label -> bool
    val pp_label : Format.formatter -> label -> unit
    val show_label : label -> string

    type t =
      { label : label
      ; value : string
      }

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val show : t -> string
    val create : label -> string -> t
    val value : t -> string
    val label : Pool_common.Language.t -> t -> string
    val find_by_value : t list -> string -> t option
  end

  module Condition : sig
    module Human : sig
      type t =
        | Checkbox of Column.t
        | Select of Column.t * SelectOption.t list

      val equal : t -> t -> bool
      val show : t -> string
      val pp : Format.formatter -> t -> unit
      val column : t -> Column.t
    end

    type t =
      | Checkbox of Column.t * bool
      | Select of Column.t * SelectOption.t

    val equal : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val checkbox : Column.t -> bool -> t
    val select : Column.t -> SelectOption.t -> t
    val show : t -> string
    val column : t -> Column.t
  end

  type human = Condition.Human.t list

  val show_human : human -> string

  type t = Condition.t list

  val equal : t -> t -> bool
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

type t =
  { filter : Filter.t option
  ; pagination : Pagination.t option
  ; search : Search.t option
  ; sort : Sort.t option
  }

val show : t -> string

val to_uri_query
  :  ?additional_params:(Pool_message.Field.t * string) list
  -> t
  -> (string * string list) list

val with_sort_order : Sort.SortOrder.t -> t -> t
val with_sort_column : Column.t -> t -> t

val create
  :  ?filter:Filter.t
  -> ?pagination:Pagination.t
  -> ?search:Search.t
  -> ?sort:Sort.t
  -> unit
  -> t

val from_request
  :  ?filterable_by:Filter.human
  -> ?searchable_by:Column.t list
  -> ?sortable_by:Column.t list
  -> ?default:t
  -> Rock.Request.t
  -> t

val empty : unit -> t

val append_query_to_sql
  :  Database.Dynparam.t
  -> string option
  -> t option
  -> Database.Dynparam.t * string * string option

val collect_and_count
  :  Database.Label.t
  -> t option
  -> select:(?count:bool -> string -> string)
  -> ?where:string * Database.Dynparam.t
  -> 'a Caqti_type.t
  -> ('a list * t) Lwt.t

val set_page_count : t -> int -> t
