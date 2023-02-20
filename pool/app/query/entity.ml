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
    ; page_count : PageCount.t (* TODO[timhub]: Maybe use a separate type *)
    }
  [@@deriving eq, show]

  let create ?limit ?page ?(page_count = 0) () =
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
    let page_count = to_int ((of_int row_count /. of_int t.limit) + 0.5) in
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
end

type t =
  { pagination : Pagination.t option
  ; search : Search.t option
  }
[@@deriving eq, show]

let pagination { pagination; _ } = pagination
let search { search; _ } = search
let create ?pagination ?search () = { pagination; search } [@@deriving eq, show]

let set_page_count { pagination; search } row_count =
  let pagination =
    pagination |> CCOption.map (Pagination.set_page_count row_count)
  in
  { pagination; search }
;;
