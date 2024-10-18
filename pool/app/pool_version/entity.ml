open Ppx_yojson_conv_lib.Yojson_conv

let equal_ptime a b = Sihl.Configuration.is_test () || Ptime.equal a b

module Id = struct
  include Pool_common.Id
end

module Version = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Version

  let create m =
    match CCString.split_on_char '.' m with
    | [ _; _; _ ] -> Ok m
    | _ -> Error Pool_message.Error.(Invalid field)
  ;;

  let schema = schema ~validation:create Pool_message.Field.Version
end

module Text = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Text
  let schema = schema ~validation:create field
end

module PublishedAt = struct
  include Pool_model.Base.Ptime

  let field = Pool_message.Field.PublishedAt
  let equal = equal_ptime
  let schema = schema field CCResult.return
end

type t =
  { id : Id.t
  ; version : Version.t
  ; text : Text.t
  ; published_at : PublishedAt.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show, yojson]

let create ?(id = Id.create ()) version text =
  { id
  ; version
  ; text
  ; published_at = None
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

open Pool_message

let filterable_by = None

let column_version =
  (Field.Version, "pool_versions.version") |> Query.Column.create
;;

let searchable_by = [ column_version ]
let sortable_by = [ column_version ]

let default_sort =
  Query.Sort.{ column = column_version; order = SortOrder.Descending }
;;

let default_query = Query.create ~sort:default_sort ()
