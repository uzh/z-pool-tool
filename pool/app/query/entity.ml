module Common = Pool_common
module Message = Common.Message

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

    let default = 0
    let field = Message.Field.Page
    let create m = if m >= 0 then Ok m else Error (Message.Invalid field)
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

  (* TODO[timhub]: make sure rounding is correct *)
  let set_page_count row_count t =
    let page_count = row_count / t.limit in
    { t with page_count }
  ;;

  let query_to_sql { limit; page; _ } =
    let offset = limit * page in
    Format.asprintf "LIMIT %i OFFSET %i " limit offset
  ;;
end

type t = { pagination : Pagination.t option } [@@deriving eq, show]

let pagination { pagination; _ } = pagination
let create ?pagination () = { pagination }

let set_page_count t row_count =
  let pagination =
    t.pagination |> CCOption.map (Pagination.set_page_count row_count)
  in
  { pagination }
;;
