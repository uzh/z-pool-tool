module Pagination : sig
  module Limit : sig
    include Pool_common.Model.IntegerSig
  end

  module Page : sig
    include Pool_common.Model.IntegerSig

    val default : t
  end

  module PageCount : sig
    include Pool_common.Model.IntegerSig
  end

  type t =
    { limit : Limit.t
    ; page : Page.t
    ; page_count : PageCount.t
    }
end

module Search : sig
  module Query : sig
    include Pool_common.Model.StringSig
  end

  type t =
    { query : Query.t
    ; columns : string list
    }
end

type t =
  { pagination : Pagination.t option
  ; search : Search.t option
  }

val show : t -> string
val from_request : ?searchable_columns:string list -> Rock.Request.t -> t
val empty : unit -> t

val append_query_to_sql
  :  Utils.Database.Dynparam.t
  -> string option
  -> t option
  -> Utils.Database.Dynparam.t * string * string option

val collect_and_count
  :  Pool_database.Label.t
  -> t option
  -> select:(string -> string)
  -> count:(string -> string)
  -> 'a Caqti_type.t
  -> ('a list * t) Lwt.t

val set_page_count : t -> int -> t
