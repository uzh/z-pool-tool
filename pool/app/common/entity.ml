module Email = Entity_email

module Password = struct
  type t = string [@@deriving eq, show]
end

module PasswordConfirmed = struct
  type t = string [@@deriving eq, show]
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
