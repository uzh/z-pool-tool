module Id = struct
  include Pool_common.Id
end

module Name = struct
  include Pool_model.Base.String

  let field = Pool_message.Field.Name
  let schema () = schema field ()
end

module Token = struct
  include Pool_model.Base.String

  let length = 32

  let charset =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_."
  ;;

  let generate () =
    let charset_length = CCString.length charset in
    let random_string = Bytes.create length in
    for i = 0 to length - 1 do
      let index = Random.int charset_length in
      Bytes.set random_string i (String.get charset index)
    done;
    Bytes.to_string random_string
  ;;

  let value m = m
end

type t =
  { id : Id.t
  ; name : Name.t
  ; token : Token.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show, yojson]

let create ?(id = Id.create ()) ?(token = Token.generate ()) name =
  { id
  ; name
  ; token
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

open Pool_message

let filterable_by = None
let column_name = (Field.Start, "pool_api_keys.title") |> Query.Column.create

let column_created_at =
  (Field.CreatedAt, "pool_api_keys.created_at") |> Query.Column.create
;;

let searchable_by = []
let sortable_by = [ column_name; column_created_at ]

let default_sort =
  Query.Sort.{ column = column_created_at; order = SortOrder.Descending }
;;

let default_query = Query.create ~sort:default_sort ()
