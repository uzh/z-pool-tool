module Id = struct
  include Pool_common.Id
end

module File = struct
  include Pool_common.File
end

module Label = struct
  let printing m fmt _ = Format.pp_print_string fmt m

  type t =
    | Direction [@name "direction"] [@printer printing "direction"]
    | AdditionalInformation [@name "additional_information"]
        [@printer printing "additional_information"]
  [@@deriving eq, show { with_path = false }, yojson, sexp_of]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;
end

module Description = struct
  type t = string [@@deriving eq, show]
end

type file =
  { id : Id.t
  ; label : Label.t
  ; language : Pool_common.Language.t
  ; description : Description.t
  ; file : File.t
  }
[@@deriving eq, show]
