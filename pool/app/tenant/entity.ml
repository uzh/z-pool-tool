module Common = Pool_common
module Id = Common.Id
module CreatedAt = Common.CreatedAt
module UpdatedAt = Common.UpdatedAt
module PoolError = Common.Message

module StatusReport = struct
  module CreatedAt = struct
    type t = Ptime.t [@@deriving eq, show]
  end

  type t = { created_at : CreatedAt.t } [@@deriving eq, show]
end
