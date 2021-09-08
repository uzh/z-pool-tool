module Email = Entity_email

module Id = struct
  type t = string

  let create () = Uuidm.create `V4 |> Uuidm.to_string
end

module Password = struct
  type t = string [@@deriving eq]

  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;
end

module PasswordConfirmed = struct
  type t = string [@@deriving eq]

  let show m = CCString.repeat "*" @@ CCString.length m

  let pp (formatter : Format.formatter) (m : t) : unit =
    Format.fprintf formatter "%s" m
  ;;
end

module Firstname = struct
  type t = string [@@deriving eq, show]
end

module Lastname = struct
  type t = string [@@deriving eq, show]
end

module Paused = struct
  type t = bool [@@deriving eq, show]
end

module Disabled = struct
  type t = bool [@@deriving eq, show]
end

module TermsAccepted = struct
  type t = Ptime.t [@@deriving eq, show]
end

module Verified = struct
  type t = Ptime.t [@@deriving eq, show]
end
