module Mapping = Entity_file_mapping
module Conformist = Pool_common.Utils.PoolConformist
module Utils = Pool_common.Utils
module Message = Pool_common.Message
module Field = Message.Field
module Address = Entity_address

let create_if_not_empty_string field m =
  if CCString.is_empty m then Error Message.(Invalid field) else Ok m
;;

module Id = struct
  include Pool_common.Id
end

module Name = struct
  type t = string [@@deriving eq, show]

  let field = Field.Name
  let create = create_if_not_empty_string field
  let value m = m
  let of_string m = m
  let schema () = Utils.schema_decoder create value field
end

module Description = struct
  type t = string [@@deriving eq, show]

  let field = Field.Description
  let create = create_if_not_empty_string field
  let value m = m
  let of_string m = m
  let schema () = Utils.schema_decoder create value field
end

module Link = struct
  type t = string [@@deriving eq, show]

  let field = Field.Link
  let create = create_if_not_empty_string field
  let value m = m
  let of_string m = m
  let schema () = Utils.schema_decoder create value field
end

module Status = struct
  let field = Pool_common.Message.Field.Status
  let go m fmt _ = Format.pp_print_string fmt m

  type t =
    | Active [@name "active"] [@printer go "active"]
    | Maintenance [@name "maintenance"] [@printer go "maintenance"]
    | Closed [@name "closed"] [@printer go "closed"]
  [@@deriving enum, eq, show { with_path = false }, yojson, sexp_of]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let create m =
    try Ok (read m) with
    | _ -> Error Pool_common.Message.(Invalid field)
  ;;

  let init = Active

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "Location Status: Could not create list of all keys!"
  ;;

  let schema () = Utils.schema_decoder create show field
end

type t =
  { id : Id.t
  ; name : Name.t
  ; description : Description.t option
  ; address : Address.t
  ; link : Link.t option
  ; status : Status.t
  ; files : Mapping.file list
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving show]

let equal m k =
  Id.equal m.id k.id
  && Name.equal m.name k.name
  && Address.equal m.address k.address
  && Status.equal m.status k.status
;;

let create ?(id = Id.create ()) name description address link status files =
  let open CCResult in
  let* name = Name.create name in
  let* description = description |> CCResult.opt_map Description.create in
  let* link = link |> CCResult.opt_map Link.create in
  Ok
    { id
    ; name
    ; description
    ; address
    ; link
    ; status
    ; files
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;
