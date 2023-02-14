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

type t = { pagination : Pagination.t option }

val from_request : Rock.Request.t -> t
val empty : unit -> t
val append_pagination_to_sql : t option -> string -> string
val set_page_count : t -> int -> t
