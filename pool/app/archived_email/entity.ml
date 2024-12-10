module Reason = struct
  module Core = struct
    let field = Pool_message.Field.MessageChannel

    type t =
      | MergedDuplicate [@name "merged_duplicate"]
      [@printer Utils.ppx_printer "merged_duplicate"]
    [@@deriving enum, eq, ord, sexp_of, show { with_path = false }, yojson]
  end

  include Pool_model.Base.SelectorType (Core)
  include Core
end
