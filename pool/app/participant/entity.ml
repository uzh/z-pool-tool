module Email = struct
  type t = string [@@deriving eq, show]
end

module Password = struct
  type t = string [@@deriving eq, show]
end

module Firstname = struct
  type t = string [@@deriving eq, show]
end

module Lastname = struct
  type t = string [@@deriving eq, show]
end

module RecruitmentChannel = struct
  type t =
    | Friend
    | Online
    | Lecture
    | Mailing
  [@@deriving eq, show]
end

type participant =
  { user : Sihl.User.t
  ; email : Email.t
  ; password : Password.t
  ; firstname : Firstname.t
  ; lastname : Lastname.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Sihl.timestamp
  }
[@@deriving eq, show]
