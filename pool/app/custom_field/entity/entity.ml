module Id = struct
  include Pool_common.Id
end

module Model = struct
  let print m fmt _ = Format.pp_print_string fmt m

  type _ t =
    | Contact : Contact.t -> [> `Single ] t [@printer print "contact"]
    | Experiment : Experiment.t -> [> `Single ] t [@printer print "experiment"]
    | Session : Session.t -> [> `Single ] t [@printer print "session"]
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
  type t = bool [@@deriving eq, show]
end

module Admin = struct
  type t =
    { view_only : bool
    ; inpup_only : bool
    ; hint : string
    ; override : bool
    }
  [@@deriving eq, show]
end

module Disabled = struct
  type t = bool [@@deriving eq, show]
end

module Field = struct
  type 'a t =
    { id : Id.t
    ; model : 'a Model.t
    ; name : Name.t
    ; hint : Hint.t
    ; field_type : FieldType.t
    ; validation : Validation.t
    ; required : Required.t
    ; metadata : Metadata.t
    ; admin : Admin.t
    ; disabled : Disabled.t
    }
end
