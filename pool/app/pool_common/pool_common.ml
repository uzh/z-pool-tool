module Id = struct
  type t = string [@@deriving eq, show]

  let create () = Uuidm.create `V4 |> Uuidm.to_string
  let of_string m = m
  let value m = m
end

module CreatedAt = struct
  type t = Ptime.t [@@deriving eq, show]

  let create = Ptime_clock.now
  let value t = t
end

module UpdatedAt = struct
  type t = Ptime.t [@@deriving eq, show]

  let create = Ptime_clock.now
  let value t = t
end

module Repo = struct
  module Id = struct
    include Id

    let t = Caqti_type.string
  end

  module UpdatedAt = struct
    include UpdatedAt

    let t = Caqti_type.ptime
  end

  module CreatedAt = struct
    include CreatedAt

    let t = Caqti_type.ptime
  end
end
