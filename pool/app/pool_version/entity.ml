open Ppx_yojson_conv_lib.Yojson_conv

module Id = struct
  include Pool_common.Id
end

module Tag = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Tag

  let create m =
    let error = Pool_message.Error.(Invalid field) in
    match CCString.split_on_char '.' m with
    | [ major; minor; patch ] ->
      [ major; minor; patch ]
      |> CCList.map CCFun.(CCInt.of_string %> CCOption.to_result error)
      |> CCList.all_ok
      |> CCResult.map (CCFun.const m)
    | _ -> Error error
  ;;

  let schema = schema ~validation:create field
end

module Text = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Text
  let schema = schema ~validation:create field
end

module PublishedAt = struct
  include Pool_model.Time

  let field = Pool_message.Field.PublishedAt
  let schema = schema field CCResult.return
end

type t =
  { id : Id.t
  ; tag : Tag.t
  ; text : Text.t
  ; published_at : PublishedAt.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show, yojson]

let create ?(id = Id.create ()) tag text =
  { id
  ; tag
  ; text
  ; published_at = None
  ; created_at = Pool_common.CreatedAt.now ()
  ; updated_at = Pool_common.UpdatedAt.now ()
  }
;;

open Pool_message

let filterable_by = None
let column_tag = (Field.Tag, "pool_versions.tag") |> Query.Column.create
let searchable_by = [ column_tag ]
let sortable_by = [ column_tag ]
let default_sort = Query.Sort.{ column = column_tag; order = SortOrder.Descending }
let default_query = Query.create ~sort:default_sort ()
