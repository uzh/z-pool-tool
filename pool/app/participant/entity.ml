type recruitment_channel =
  | Friend
  | Online
  | Lecture
  | Mailing
[@@deriving eq, show]

type participant =
  { user : Sihl.User.t
  ; email : string
  ; password : string
  ; firstname : string
  ; lastname : string
  ; recruitment_channel : recruitment_channel
  ; terms_accepted_at : Sihl.timestamp
  }
[@@deriving eq, show]
