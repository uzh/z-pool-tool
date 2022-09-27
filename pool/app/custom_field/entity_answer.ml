module Id = struct
  include Pool_common.Id
end

module Answer = struct
  type t =
    | Text of string
    | Number of int
  [@@deriving eq, show, yojson]
end

type t =
  { id : Id.t
  ; answer : Answer.t
  ; version : Pool_common.Version.t
  }
[@@deriving eq, show]
