module ToHandle = struct
  include Pool_model.Base.Integer

  let field = Pool_message.Field.ToHandle

  let create m =
    if m > 0 then Ok m else Error Pool_message.(Error.Invalid field)
  ;;

  let schema = schema field create
end

module LastRun = struct
  include Pool_model.Base.Boolean

  let schema = schema Pool_message.Field.LastRun
end

type status =
  { mailing : Entity.t
  ; to_handle : ToHandle.t
  ; last_run : LastRun.t
  }
[@@deriving eq, show]

type t = status [@@deriving eq, show]

let to_handle m = m.to_handle
