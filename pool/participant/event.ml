type update =
  { firstname : string
  ; lastname : string
  ; paused : string
  }

type t =
  [ `Signed_up of Sihl.User.t
  | `Updated_details of update
  | `Updated_email of string
  | `Updated_password of string
  ]
