module Common = Pool_common
module Message = Common.Message
module Dynparam = Utils.Database.Dynparam

module Pagination = struct
  module Limit = struct
    include Common.Model.Integer

    let default = 20
    let field = Message.Field.Limit
    let create m = if m >= 0 then Ok m else Error (Message.Invalid field)
    let schema = schema field create
  end

  module Page = struct
    include Common.Model.Integer

    let default = 1
    let field = Message.Field.Page
    let create m = if m > 0 then Ok m else Error (Message.Invalid field)
    let schema = schema field create
  end

  module PageCount = struct
    include Common.Model.Integer

    let field = Message.Field.PageCount
    let create m = if m >= 0 then Ok m else Error (Message.Invalid field)
    let schema = schema field create
  end

  type t =
    { limit : Limit.t
    ; page : Page.t
    ; page_count : PageCount.t
    }
  [@@deriving eq, show]

  let create ?limit ?page ?(page_count = 1) () =
    let open CCOption in
    let open CCFun in
    let get_value = value in
    let build input create default =
      input |> map_or ~default (create %> of_result %> get_value ~default)
    in
    let page = Page.(build page create default) in
    let limit = value ~default:Limit.default limit in
    { limit; page; page_count }
  ;;

  let set_page_count row_count t =
    let open CCFloat in
    let page_count =
      ceil (of_int row_count /. of_int t.limit)
      |> to_int
      |> function
      | 0 -> 1
      | i -> i
    in
    { t with page_count }
  ;;

  let to_sql { limit; page; _ } =
    let offset = limit * (page - 1) in
    Format.asprintf "LIMIT %i OFFSET %i " limit offset
  ;;
end

module Search = struct
  module Query = struct
    include Common.Model.String

    let field = Common.Message.Field.Query
    let schema = schema ?validation:None field
    let of_string m = m
  end

  type t =
    { query : Query.t
    ; columns : string list
    }
  [@@deriving eq, show]

  let create query columns = { query; columns }

  let to_sql dyn { columns; query } =
    match columns with
    | [] -> dyn, None
    | columns ->
      let dyn, where =
        CCList.fold_left
          (fun (dyn, columns) column ->
            ( dyn |> Dynparam.add Caqti_type.string ("%" ^ query ^ "%")
            , Format.asprintf "%s LIKE ? " column :: columns ))
          (dyn, [])
          columns
      in
      where
      |> CCString.concat ") OR ("
      |> Format.asprintf "(%s)"
      |> fun where -> dyn, Some where
  ;;

  let query_string t = t.query |> Query.value
end

module Sort = struct
  module SortOrder = struct
    include Pool_common.SortOrder

    let to_human lang =
      let open CCFun in
      let open Pool_common in
      (function
       | Ascending -> Message.Ascending
       | Descending -> Message.Descending)
      %> Utils.control_to_string lang
    ;;
  end

  type t =
    { column : Pool_common.Message.Field.t * string
    ; order : SortOrder.t
    }
  [@@deriving eq, show]

  let create sortable_columns ?(order = SortOrder.default) field =
    CCList.find_opt
      (fun column -> column |> fst |> Pool_common.Message.Field.equal field)
      sortable_columns
    |> CCOption.map (fun column -> { column; order })
  ;;

  let to_sql { column; order } =
    Format.asprintf "%s %s" (snd column) (SortOrder.show order)
  ;;
end

type t =
  { pagination : Pagination.t option
  ; search : Search.t option
  ; sort : Sort.t option
  }
[@@deriving eq, show]

let pagination { pagination; _ } = pagination
let search { search; _ } = search
let sort { sort; _ } = sort
let create ?pagination ?search ?sort () = { pagination; search; sort }

let set_page_count ({ pagination; _ } as t) row_count =
  let pagination =
    pagination |> CCOption.map (Pagination.set_page_count row_count)
  in
  { t with pagination }
;;
