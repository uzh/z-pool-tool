module ToHandle = struct
  include Pool_common.Model.Integer

  let field = Pool_common.Message.Field.ToHandle

  let create m =
    if m > 0 then Ok m else Error Pool_common.Message.(Invalid field)
  ;;

  let schema = schema field create
end

module LastRun = struct
  include Pool_common.Model.Boolean

  let schema = schema Pool_common.Message.Field.LastRun
end

type status =
  { mailing : Entity.t
  ; to_handle : ToHandle.t
  ; last_run : LastRun.t
  }
[@@deriving eq, show]

type t = status [@@deriving eq, show]

let to_handle m = m.to_handle
