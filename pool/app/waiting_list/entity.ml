module Id = struct
  include Pool_common.Id

  let to_common m = m
end

module AdminComment = struct
  type t = string [@@deriving eq, show]

  let create m = m
  let value m = m

  let schema () =
    Pool_common.(
      Utils.schema_decoder
        (fun m -> Ok (m |> create))
        value
        Message.Field.AdminComment)
  ;;
end

type t =
  { id : Id.t
  ; contact : Contact.t
  ; experiment : Experiment.t
  ; admin_comment : AdminComment.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show, fields ~getters]

let create ?(id = Pool_common.Id.create ()) contact experiment admin_comment =
  { id
  ; contact
  ; experiment
  ; admin_comment
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;

let column_signed_up_at =
  Pool_common.Message.(Field.SignedUpAt, "pool_waiting_list.created_at")
  |> Query.Column.create
;;

let default_sort =
  let open Query in
  Sort.{ column = column_signed_up_at; order = SortOrder.Ascending }
;;

let filterable_by = None
let searchable_by = Contact.searchable_by
let sortable_by = searchable_by @ [ column_signed_up_at ]
let default_query = Query.create ~sort:default_sort ()
