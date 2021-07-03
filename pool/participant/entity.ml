type recruitment_channel =
  | Friend
  | Online
  | Lecture
  | Mailing

type participant =
  { email : string
  ; password : string
  ; firstname : string
  ; lastname : string
  ; recruitment_channel : recruitment_channel
  ; terms_accepted_at : Sihl.timestamp
  }
