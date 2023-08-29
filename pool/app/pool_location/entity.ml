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
  open Ppx_yojson_conv_lib.Yojson_conv

  type t = (Pool_common.Language.t * string) list [@@deriving eq, show, yojson]

  let pp ppf =
    CCList.iter (fun (key, value) ->
      Format.fprintf ppf "[%s: %s]." (Pool_common.Language.show key) value)
  ;;

  let field = Field.Description

  let field_name langauge =
    Format.asprintf
      "%s[%s]"
      Message.Field.(show field)
      (Pool_common.Language.show langauge)
  ;;

  let find_opt = CCList.assoc_opt ~eq:Pool_common.Language.equal

  let create sys_languages descriptions =
    CCList.filter
      (fun lang ->
        CCList.assoc_opt ~eq:Pool_common.Language.equal lang descriptions
        |> CCOption.is_none)
      sys_languages
    |> function
    | [] -> Ok descriptions
    | _ -> Error Message.(AllLanguagesRequired field)
  ;;

  let read yojson =
    try Ok (yojson |> Yojson.Safe.from_string |> t_of_yojson) with
    | _ -> Error Pool_common.Message.(Invalid field)
  ;;

  let value m = m
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
  CCString.concat ", " (Address.address_rows_human language location.address)
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

let file_path file =
  Format.asprintf "files/%s" Mapping.(Id.value file.Mapping.id)
;;

let contact_file_path id file =
  Format.asprintf "/location/%s/%s" (Id.value id) (file_path file)
;;

let admin_file_path id file =
  Format.asprintf "/admin/locations/%s/%s" (Id.value id) (file_path file)
;;

module Human = struct
  let link_with_default ~default { link; _ } =
    link |> CCOption.map_or ~default Link.value
  ;;

  let detailed language { description; address; _ } =
    let concat = CCString.concat "\n" in
    let address = Address.address_rows_human language address in
    let address_block = address |> concat in
    let description =
      CCOption.bind description (Description.find_opt language)
    in
    match description with
    | None -> address_block
    | Some description -> [ address_block; ""; description ] |> concat
  ;;

  let description langauge { description; _ } =
    CCOption.bind description (Description.find_opt langauge)
  ;;
end
