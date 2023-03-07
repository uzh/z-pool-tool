module Mapping = Entity_file_mapping
module Conformist = Pool_common.Utils.PoolConformist
module Message = Pool_common.Message
module Field = Message.Field
module Address = Entity_address

module Id = struct
  include Pool_common.Id
end

module Name = struct
  include Pool_common.Model.String

  let field = Field.Name
  let schema () = schema field ()
end

module Description = struct
  include Pool_common.Model.String

  let field = Field.Description
  let schema () = schema field ()
end

module Link = struct
  include Pool_common.Model.String

  let field = Field.Link
  let schema () = schema field ()
end

module Status = struct
  let print = Utils.ppx_printer

  module Core = struct
    let field = Pool_common.Message.Field.Status

    type t =
      | Active [@name "active"] [@printer print "active"]
      | Maintenance [@name "maintenance"] [@printer print "maintenance"]
      | Closed [@name "closed"] [@printer print "closed"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core

  let init = Active
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

let to_string language location =
  CCString.concat
    ", "
    (Address.address_rows_human language location.address
     |> fun (room, street, city) ->
     [ room; street; city ] |> CCList.filter CCFun.(CCString.is_empty %> not))
;;

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
