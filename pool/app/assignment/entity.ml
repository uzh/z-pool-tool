module ShowUp = struct
  include Pool_common.Model.Boolean

  let init = false
  let schema = schema Pool_common.Message.Field.ShowUp
end

module Participated = struct
  include Pool_common.Model.Boolean

  let init = false
  let schema = schema Pool_common.Message.Field.Participated
end

module MatchesFilter = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
  let init = false
end

module CanceledAt = struct
  type t = Ptime.t [@@deriving eq, show]

  let create_now () = Ptime_clock.now ()
  let value m = m
  let sexp_of_t = Pool_common.Utils.Time.ptime_to_sexp
end

type t =
  { id : Pool_common.Id.t
  ; contact : Contact.t
  ; show_up : ShowUp.t
  ; participated : Participated.t
  ; matches_filter : MatchesFilter.t
  ; canceled_at : CanceledAt.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create
  ?(id = Pool_common.Id.create ())
  ?(show_up = ShowUp.create false)
  ?(participated = Participated.create false)
  ?(matches_filter = MatchesFilter.create true)
  ?canceled_at
  contact
  =
  { id
  ; contact
  ; show_up
  ; participated
  ; matches_filter
  ; canceled_at
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

module Public = struct
  type t =
    { id : Pool_common.Id.t
    ; canceled_at : CanceledAt.t option
    }
end
