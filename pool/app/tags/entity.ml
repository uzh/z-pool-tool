open CCFun.Infix
module Message = Pool_common.Message
module Ptime = Pool_common.Model.Ptime
module Id = Pool_common.Id

let printer = Utils.ppx_printer

module Model = struct
  module Core = struct
    let field = Message.Field.Model

    type t =
      | Contact [@name "contact"] [@printer printer "contact"]
      | Experiment [@name "experiment"] [@printer printer "experiment"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_common.Model.SelectorType (Core)
  include Core
end

module Title = struct
  include Pool_common.Model.String

  let field = Message.Field.Title
  let schema () = schema field ()
end

module Description = struct
  include Pool_common.Model.String

  let field = Message.Field.Description
  let schema () = schema field ()
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t option
  ; model : Model.t
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) ?description title model =
  let open CCResult in
  let* description =
    CCOption.map_or
      ~default:(Ok None)
      (Description.create %> map CCOption.return)
      description
  in
  let* title = Title.create title in
  Ok { id; title; description; model }
;;

module Tagged = struct
  type t =
    { model_uuid : Pool_common.Id.t
    ; tag_uuid : Id.t
    }
  [@@deriving eq, show]

  let create model_uuid tag_uuid = Ok { model_uuid; tag_uuid }
end
