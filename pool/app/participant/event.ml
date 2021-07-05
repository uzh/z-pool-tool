type update =
  { email : string
  ; firstname : string
  ; lastname : string
  ; paused : string
  }

type event =
  [ `Created of Entity.participant
  | `Details_updated of Entity.participant * update
  | `Password_updated of Entity.participant * string
  | `Email_updated of Entity.participant * string
  ]

type handle_event = event -> unit Lwt.t

let handle_event : handle_event =
  let open Lwt.Syntax in
  function
  | `Created participant ->
    let* () = Sihl.User.create participant.user in
    Repo.insert participant
  | `Details_updated user -> Repo.update user
  | `Password_updated (_, _) -> Sihl.todo
  | `Email_updated (_, _) -> Sihl.todo
;;
