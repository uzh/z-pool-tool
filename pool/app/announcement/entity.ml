open Ppx_yojson_conv_lib.Yojson_conv
module Language = Pool_common.Language

let ptime_schema field =
  Pool_conformist.schema_decoder
    Pool_model.Time.parse_time
    Ptime.to_rfc3339
    field
;;

module Id = struct
  include Pool_common.Id
end

module Text = struct
  type name = string [@@deriving eq, show, yojson]

  let value_name n = n

  type t = (Language.t * name) list [@@deriving eq, show, yojson]

  let find_opt lang t = CCList.assoc_opt ~eq:Language.equal lang t

  let find lang t =
    find_opt lang t |> CCOption.value ~default:(CCList.hd t |> snd)
  ;;

  let create = function
    | [] -> Error Pool_message.(Error.AtLeastOneLanguageRequired Field.Text)
    | names -> Ok names
  ;;
end

module StartAt = struct
  include Pool_model.Base.Ptime

  let create m = m
  let schema () = ptime_schema Pool_message.Field.Start
end

module EndAt = struct
  include Pool_model.Base.Ptime

  let create m = m
  let schema () = ptime_schema Pool_message.Field.End
end

type t =
  { id : Id.t
  ; text : Text.t
  ; start_at : StartAt.t option
  ; end_at : EndAt.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show, yojson]

let create ?(id = Id.create ()) tet start_at end_at =
  { id
  ; text = tet
  ; start_at
  ; end_at
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

open Pool_message

let filterable_by = None

let column_start =
  (Field.Start, "pool_announcements.start_at") |> Query.Column.create
;;

let column_end = (Field.End, "pool_announcements.end_at") |> Query.Column.create
let searchable_by = []
let sortable_by = [ column_start; column_end ]

let default_sort =
  Query.Sort.{ column = column_start; order = SortOrder.Descending }
;;

let default_query = Query.create ~sort:default_sort ()
