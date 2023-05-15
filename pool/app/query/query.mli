module Column : sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val field : t -> Pool_common.Message.Field.t
  val create_list : (Pool_common.Message.Field.t * string) list -> t list
end

module Pagination : sig
  module Limit : Pool_common.Model.IntegerSig

  module Page : sig
    include Pool_common.Model.IntegerSig

    val default : t
  end

  module PageCount : Pool_common.Model.IntegerSig

  type t =
    { limit : Limit.t
    ; page : Page.t
    ; page_count : PageCount.t
    }
end

module Search : sig
  module Query : Pool_common.Model.StringSig

  type t =
    { query : Query.t
    ; columns : Column.t list
    }

  val query_string : t -> string
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
    val create : string -> (t, Pool_common.Message.error) result
    val all : t list
    val default : t

    val schema
      :  unit
      -> (Pool_common.Message.error, t) Pool_common.Utils.PoolConformist.Field.t
  end

  type t =
    { column : Column.t
    ; order : SortOrder.t
    }
end

type t =
  { pagination : Pagination.t option
  ; search : Search.t option
  ; sort : Sort.t option
  }

val show : t -> string

val from_request
  :  ?searchable_by:Column.t list
  -> ?sortable_by:Column.t list
  -> Rock.Request.t
  -> t

val empty : unit -> t

val append_query_to_sql
  :  Utils.Database.Dynparam.t
  -> string option
  -> t option
  -> Utils.Database.Dynparam.t * string * string option

val collect_and_count
  :  Pool_database.Label.t
  -> t option
  -> ?having_dynparam:(Utils.Database.Dynparam.t -> Utils.Database.Dynparam.t)
  -> select:(string -> string)
  -> count:(string -> string)
  -> ?where:string * Utils.Database.Dynparam.t
  -> 'a Caqti_type.t
  -> ('a list * t) Lwt.t

val set_page_count : t -> int -> t
