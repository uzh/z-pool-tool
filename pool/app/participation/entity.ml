module ShowUp = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
  let init = false

  let schema () =
    Pool_common.Utils.schema_decoder
      (fun m ->
        m
        |> bool_of_string_opt
        |> CCOption.get_or ~default:false
        |> CCResult.pure)
      string_of_bool
      Pool_common.Message.Field.ShowUp
  ;;
end

module Participated = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
  let init = false

  let schema () =
    Pool_common.Utils.schema_decoder
      (fun m ->
        m
        |> bool_of_string_opt
        |> CCOption.get_or ~default:false
        |> CCResult.pure)
      string_of_bool
      Pool_common.Message.Field.Participated
  ;;
end

module MatchesFilter = struct
  type t = bool [@@deriving eq, show]

  let create m = m
  let value m = m
  let init = false
end

module CanceledAt = struct
  type t = Ptime.t option [@@deriving eq, show]

  let init = None
  let create_now () = Some (Ptime_clock.now ())
  let value m = m
  let sexp_of_t = Pool_common.Utils.Time.ptime_to_sexp
end

type t =
  { id : Pool_common.Id.t
  ; participant : Participant.t
  ; show_up : ShowUp.t
  ; participated : Participated.t
  ; matches_filter : MatchesFilter.t
  ; canceled_at : CanceledAt.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create
    ?(id = Pool_common.Id.create ())
    ?(show_up = ShowUp.create false)
    ?(participated = Participated.create false)
    ?(matches_filter = MatchesFilter.create true)
    ?(canceled_at = CanceledAt.init)
    participant
  =
  { id
  ; participant
  ; show_up
  ; participated
  ; matches_filter
  ; canceled_at
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;
