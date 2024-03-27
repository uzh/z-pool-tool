module StatusReport = struct
  module CreatedAt = struct
    include Pool_model.Base.Ptime
  end

  type t = { created_at : CreatedAt.t } [@@deriving eq, show]
end
