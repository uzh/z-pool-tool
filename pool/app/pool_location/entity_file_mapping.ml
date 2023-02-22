module Conformist = Pool_common.Utils.PoolConformist

module Id = struct
  include Pool_common.Id
end

module File = struct
  include Pool_common.File
end

module Label = struct
  let print = Utils.ppx_printer

  module Core = struct
    let field = Pool_common.Message.Field.Label

    type t =
      | Direction [@name "direction"] [@printer print "direction"]
      | AdditionalInformation [@name "additional_information"]
          [@printer print "additional_information"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core
end

type file =
  { id : Id.t
  ; label : Label.t
  ; language : Pool_common.Language.t
  ; file : File.t
  }
[@@deriving eq, show]

type file_base =
  { label : Label.t
  ; language : Pool_common.Language.t
  ; asset_id : Pool_common.Id.t
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
  [@@deriving eq, show]

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
