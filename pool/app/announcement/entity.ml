open Ppx_yojson_conv_lib.Yojson_conv
module Language = Pool_common.Language

let ptime_schema field =
  Pool_conformist.schema_decoder
    Pool_model.Time.parse_time
    Ptime.to_rfc3339
    field
;;

let equal_ptime a b = Sihl.Configuration.is_test () || Ptime.equal a b

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

  let equal a b =
    let sort = CCList.sort (fun (a, _) (b, _) -> Language.compare a b) in
    if Sihl.Configuration.is_test () then equal (sort a) (sort b) else equal a b
  ;;
end

module StartAt = struct
  include Pool_model.Base.Ptime

  let create m = m
  let schema () = ptime_schema Pool_message.Field.Start
  let equal = equal_ptime
end

module EndAt = struct
  include Pool_model.Base.Ptime

  let create m = m
  let schema () = ptime_schema Pool_message.Field.End
  let equal = equal_ptime
end

module ShowToAdmins = struct
  include Pool_model.Base.Boolean

  let init = false
  let field = Pool_message.Field.ShowToAdmins
  let schema ?default = schema ?default field
end

module ShowToContacts = struct
  include Pool_model.Base.Boolean

  let init = false
  let field = Pool_message.Field.ShowToContacts
  let schema ?default = schema ?default field
end

type t =
  { id : Id.t
  ; text : Text.t
  ; start_at : StartAt.t option
  ; end_at : EndAt.t option
  ; show_to_admins : ShowToAdmins.t
  ; show_to_contacts : ShowToContacts.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show, yojson]

let sexp_of_t { id; _ } = Id.sexp_of_t id

let create
  ?(id = Id.create ())
  text
  start_at
  end_at
  show_to_admins
  show_to_contacts
  =
  { id
  ; text
  ; start_at
  ; end_at
  ; show_to_admins
  ; show_to_contacts
  ; created_at = Pool_common.CreatedAt.create_now ()
  ; updated_at = Pool_common.UpdatedAt.create_now ()
  }
;;

type admin = t * Pool_tenant.t list [@@deriving eq, show]

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
