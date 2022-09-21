module Message = Pool_common.Message
module Language = Pool_common.Language

let printer m fmt _ = Format.pp_print_string fmt m

module Id = struct
  include Pool_common.Id
end

module Model = struct
  let go m fmt _ = Format.pp_print_string fmt m

  type t =
    | Contact [@name "contact"] [@printer printer "contact"]
    | Experiment [@name "experiment"] [@printer printer "experiment"]
    | Session [@name "session"] [@printer printer "session"]
  [@@deriving eq, show { with_path = false }, yojson, enum]

  let field = Pool_common.Message.Field.Model

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "Models: Could not create list of all models!"
  ;;

  let create s =
    try Ok (read s) with
    | _ -> Error Pool_common.Message.(Invalid field)
  ;;

  let value = show
  let schema () = Pool_common.Utils.schema_decoder create value field
end

module Name = struct
  (* TODO: do we need validation? e.g. primary language is set? *)
  type name = string [@@deriving eq, show, yojson]

  let value_name n = n

  type t = (Language.t * name) list [@@deriving eq, show, yojson]

  let find_opt t lang = CCList.assoc_opt ~eq:Language.equal lang t

  let create names =
    if CCList.is_empty names
    then Error Pool_common.Message.(Empty Field.Name)
    else Ok names
  ;;
end

module Hint = struct
  type hint = string [@@deriving eq, show, yojson]

  let value_hint h = h

  type t = (Language.t * hint) list [@@deriving eq, show, yojson]

  let find_opt t lang = CCList.assoc_opt ~eq:Language.equal lang t

  let create hints =
    (* TODO: do we need validation? e.g. if one hint is given, all languages of
       name need hint? *)
    Ok hints
  ;;
end

module FieldType = struct
  type t =
    | Boolean [@name "boolean"] [@printer printer "boolean"]
    | Text [@name "text"] [@printer printer "text"]
  [@@deriving eq, show { with_path = false }, yojson, enum]

  let field = Pool_common.Message.Field.FieldType

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let all : t list =
    CCList.range min max
    |> CCList.map of_enum
    |> CCList.all_some
    |> CCOption.get_exn_or "Models: Could not create list of all models!"
  ;;

  let create s =
    try Ok (read s) with
    | _ -> Error Pool_common.Message.(Invalid field)
  ;;

  let value = show
  let schema () = Pool_common.Utils.schema_decoder create value field
end

module Validation = struct
  module Regex = struct
    include Pool_common.Model.String

    let field = Message.Field.Regex
    let create = create field
    let schema = schema field ?validation:None
  end

  module Error = struct
    type t =
      | Invalid [@name "invalid"] [@printer printer "invalid"]
      | Malformatted [@name "malformatted"] [@printer printer "malformatted"]
      | NegativeAmount [@name "negative_amount"]
          [@printer printer "negative_amount"]
      | NoValue [@name "no_value"] [@printer printer "no_value"]
    [@@deriving eq, show { with_path = false }, yojson, enum]

    let all : t list =
      CCList.range min max
      |> CCList.map of_enum
      |> CCList.all_some
      |> CCOption.get_exn_or "Errors: Could not create list of all errors!"
    ;;

    let field = Pool_common.Message.Field.ErrorMessage

    let read m =
      m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
    ;;

    let create s =
      try Ok (read s) with
      | _ -> Error Pool_common.(Message.Invalid field)
    ;;

    let value = show

    let format_as_label t =
      t |> show |> CCString.replace ~which:`All ~sub:"_" ~by:" "
    ;;

    let schema () = Pool_common.Utils.schema_decoder create value field
  end

  type t =
    { regex : Regex.t
    ; error : Error.t
    }
  [@@deriving eq, show, yojson]
end

module Required = struct
  include Pool_common.Model.Boolean

  let schema = schema Pool_common.Message.Field.Required
end

module Disabled = struct
  include Pool_common.Model.Boolean

  let schema = schema Pool_common.Message.Field.Disabled
end

module Admin = struct
  module Hint = struct
    include Pool_common.Model.String

    let field = Message.Field.AdminHint
    let create = create field
    let schema = schema field ?validation:None
  end

  module Overwrite = struct
    include Pool_common.Model.Boolean

    let schema = schema Pool_common.Message.Field.Overwrite
  end

  type t =
    { hint : Hint.t option
    ; overwrite : Overwrite.t
    }
  [@@deriving eq, show]
end

type t =
  { id : Id.t
  ; model : Model.t
  ; name : Name.t
  ; hint : Hint.t
  ; field_type : FieldType.t
  ; validation : Validation.t
  ; required : Required.t
  ; disabled : Disabled.t
  ; admin : Admin.t
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create
  ?(id = Pool_common.Id.create ())
  model
  name
  hint
  field_type
  validation
  required
  disabled
  admin
  =
  let open CCResult in
  let* name = Name.create name in
  let* hint = Hint.create hint in
  Ok
    { id
    ; model
    ; name
    ; hint
    ; field_type
    ; validation
    ; required
    ; disabled
    ; admin
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;

module Write = struct
  type t =
    { id : Id.t
    ; model : Model.t
    ; name : Name.t
    ; hint : Hint.t
    ; field_type : FieldType.t
    ; validation : Validation.t
    ; required : Required.t
    ; disabled : Disabled.t
    ; admin : Admin.t
    }
  [@@deriving eq, show]
end

let boolean_fields = Pool_common.Message.Field.[ Required; Disabled; Overwrite ]

(* A field should linked to a "category" *)
(* type _ t = | Contact : field -> [> `Single ] t | Experiment : field -> [>
   `Single ] t | Session : field -> [> `Single ] t

   module Answer = struct module TextAnswer = struct type t = string [@@deriving
   eq, show, yojson] end

   module TextAnswerMasked = struct type t = string [@@deriving eq, yojson]

   let show m = CCString.repeat "*" @@ CCString.length m

   let pp (formatter : Format.formatter) (m : t) : unit = Format.fprintf
   formatter "%s" m ;; end

   type t = | Text of TextAnswer.t | Masked of TextAnswerMasked.t [@@deriving
   yojson, show, eq]

   let text = function | Text text -> text | Masked text ->
   TextAnswerMasked.show text ;; end

   module Populated = struct module AssignedModel = struct let print m fmt _ =
   Format.pp_print_string fmt m

   type _ t = | Contact : Contact.t -> [> `Single ] t [@printer print "contact"]
   | Experiment : Experiment.t -> [> `Single ] t [@printer print "experiment"] |
   Session : Session.t -> [> `Single ] t [@printer print "session"] [@@deriving
   eq, show] end

   (* TODO: Populated Entity should be linked to a fixed e.g. "Contact.t" *)
   type _ t = { id : Id.t ; name : Name.t ; hint : Hint.t ; field_type :
   FieldType.t ; validation : Validation.t ; required : Required.t ; metadata :
   Metadata.t ; disabled : Disabled.t } end *)
