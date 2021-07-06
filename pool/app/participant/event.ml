type create =
  { email : string
  ; password : string
  ; firstname : string
  ; lastname : string
  ; recruitment_channel : Entity.recruitment_channel
  ; terms_accepted_at : Sihl.timestamp
  }
[@@deriving eq, show]

type update =
  { email : string
  ; firstname : string
  ; lastname : string
  ; paused : string
  }
[@@deriving eq, show]

type event =
  [ `Created of create
  | `Details_updated of Entity.participant * update
  | `Password_updated of Entity.participant * string
  | `Email_updated of Entity.participant * string
  ]
[@@deriving eq, show]

let handle_event : event -> unit Lwt.t =
  let open Lwt.Syntax in
  function
  | `Created participant ->
    let* user =
      Sihl.User.create ~email:participant.email ~password:participant.password
    in
    let* () = Permission.assign user (Role.participant user.id) in
    Repo.insert participant
  | `Details_updated user -> Repo.update user
  | `Password_updated (participant, password) ->
    Sihl.User.set_password participant.user password
  | `Email_updated (_, _) -> Sihl.todo ()
;;
