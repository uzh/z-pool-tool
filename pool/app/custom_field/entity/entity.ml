module Id = struct
  include Pool_common.Id
end

module ModelOptions = struct
  type t =
    | Contact
    | Experiment
    | Session
  [@@deriving eq, show]
end

module Name = struct
  type t = string [@@deriving eq, show]
end

module Hint = struct
  type t = string [@@deriving eq, show]
end

module Validation = struct
  type t =
    { regex : string
    ; error : Pool_common.Message.error
    }
  [@@deriving eq, show]
end

module FieldType = struct
  type t =
    | Boolean
    | Text
  [@@deriving eq, show]
end

module Required = struct
  type t = bool [@@deriving eq, show]
end

module Metadata = struct
  module Admin = struct
    type t =
      { view_only : bool
      ; input_only : bool
      ; hint : string
      ; override : bool
      }
    [@@deriving eq, show]
  end

  type t = { admin : Admin.t option } [@@deriving eq, show]

  let empty = { admin = None }
end

module Disabled = struct
  type t = bool [@@deriving eq, show]
end

type field =
  { id : Id.t
  ; name : Name.t
  ; hint : Hint.t
  ; field_type : FieldType.t
  ; validation : Validation.t
  ; required : Required.t
  ; metadata : Metadata.t
  ; disabled : Disabled.t
  }

(* A field should linked to a "category" *)
type _ t =
  | Contact : field -> [> `Single ] t
  | Experiment : field -> [> `Single ] t
  | Session : field -> [> `Single ] t

module Answer = struct
  module TextAnswer = struct
    type t = string [@@deriving eq, show, yojson]
  end

  module TextAnswerMasked = struct
    type t = string [@@deriving eq, yojson]

    let show m = CCString.repeat "*" @@ CCString.length m

    let pp (formatter : Format.formatter) (m : t) : unit =
      Format.fprintf formatter "%s" m
    ;;
  end

  type t =
    | Text of TextAnswer.t
    | Masked of TextAnswerMasked.t
  [@@deriving yojson, show, eq]

  let text = function
    | Text text -> text
    | Masked text -> TextAnswerMasked.show text
  ;;
end

module Populated = struct
  module AssignedModel = struct
    let print m fmt _ = Format.pp_print_string fmt m

    type _ t =
      | Contact : Contact.t -> [> `Single ] t [@printer print "contact"]
      | Experiment : Experiment.t -> [> `Single ] t
          [@printer print "experiment"]
      | Session : Session.t -> [> `Single ] t [@printer print "session"]
    [@@deriving eq, show]
  end

  (* TODO: Populated Entity should be linked to a fixed e.g. "Contact.t" *)
  type _ t =
    { id : Id.t
    ; name : Name.t
    ; hint : Hint.t
    ; field_type : FieldType.t
    ; validation : Validation.t
    ; required : Required.t
    ; metadata : Metadata.t
    ; disabled : Disabled.t
    }
end
