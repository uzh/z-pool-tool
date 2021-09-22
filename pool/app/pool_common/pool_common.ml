module Id = struct
  type t = string [@@deriving eq, show]

  let create () = Uuidm.create `V4 |> Uuidm.to_string
  let of_string m = m
  let value m = m
end

module Repo = struct
  module Id = struct
    include Id

    let t = Caqti_type.string
  end
end
