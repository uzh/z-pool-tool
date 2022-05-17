module Mapping = Entity_file_mapping

module Id = struct
  include Pool_common.Id
end

module Name = struct
  type t = string [@@deriving eq, show]

  let value m = m
end

module Description = struct
  type t = string [@@deriving eq, show]

  let value m = m
end

module MailingAddress = struct
  type t = string [@@deriving eq, show]

  let value m = m
end

module Link = struct
  type t = string [@@deriving eq, show]

  let value m = m
end

module Status = struct
  let printing m fmt _ = Format.pp_print_string fmt m

  type t =
    | Active [@name "active"] [@printer printing "active"]
    | Maintenance [@name "maintenance"] [@printer printing "maintenance"]
    | Closed [@name "closed"] [@printer printing "closed"]
  [@@deriving eq, show { with_path = false }, yojson, sexp_of]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;
end

type t =
  { id : Id.t
  ; name : Name.t
  ; description : Description.t
  ; address : MailingAddress.t
  ; link : Link.t
  ; status : Status.t
  ; files : Mapping.file list
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) name description address link status files =
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
