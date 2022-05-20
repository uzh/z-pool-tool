module Conformist = Pool_common.Utils.PoolConformist

module Id = struct
  include Pool_common.Id
end

module File = struct
  include Pool_common.File
end

module Label = struct
  let field = Pool_common.Message.Field.Label
  let go m fmt _ = Format.pp_print_string fmt m

  type t =
    | Direction [@name "direction"] [@printer go "direction"]
    | AdditionalInformation [@name "additional_information"]
        [@printer go "additional_information"]
  [@@deriving eq, show { with_path = false }, yojson, sexp]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let create m =
    try Ok (read m) with
    | _ -> Error Pool_common.Message.(Invalid field)
  ;;

  let schema () = Pool_common.Utils.schema_decoder create show field

  (* TODO: Is there a better way the supressing the warning 4 for the whole
     module? *)
end [@warning "-4"]

type file =
  { id : Id.t
  ; label : Label.t
  ; language : Pool_common.Language.t
  ; file : File.t
  }
[@@deriving eq, show]

type create =
  { label : Label.t
  ; language : Pool_common.Language.t
  ; file : File.t
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) label language file =
  let open CCResult in
  let* label = Label.create label in
  Ok { id; label; language; file }
;;

module Write = struct
  type file =
    { id : Id.t
    ; label : Label.t
    ; language : Pool_common.Language.t
    ; asset_id : Id.t
    ; location_id : Pool_common.Id.t
    }
  [@@deriving eq, show, sexp]

  let create
      ?(id = Pool_common.Id.create ())
      label
      language
      asset_id
      location_id
    =
    { id; label; language; asset_id; location_id }
  ;;
end
