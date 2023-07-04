module Token = struct
  include Pool_common.Model.String

  let field = Pool_common.Message.Field.Token
  let schema () = schema field ()
end

module ConfirmedAt = struct
  include Pool_common.Model.Ptime

  let create m = Ok m
  let schema = schema Pool_common.Message.Field.ConfirmedAt create
end

module NotifiedAt = struct
  include Pool_common.Model.Ptime

  let create m = Ok m
  let schema = schema Pool_common.Message.Field.NotifiedAt create
end

module RemindedAt = struct
  include Pool_common.Model.Ptime

  let create m = Ok m
  let schema = schema Pool_common.Message.Field.RemindedAt create
end

type t =
  { user_uuid : Pool_common.Id.t
  ; token : Token.t
  ; confirmed_at : ConfirmedAt.t option
  ; notified_at : NotifiedAt.t option
  ; reminded_at : RemindedAt.t option
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]
