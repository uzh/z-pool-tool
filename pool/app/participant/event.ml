type create =
  { email : Entity.Email.t
  ; password : Entity.Password.t
  ; firstname : Entity.Firstname.t
  ; lastname : Entity.Lastname.t
  ; recruitment_channel : Entity.RecruitmentChannel.t
  ; terms_accepted_at : Sihl.timestamp
  }
[@@deriving eq, show]

type update =
  { email : Entity.Email.t
  ; firstname : Entity.Firstname.t
  ; lastname : Entity.Lastname.t
  ; paused : string
  }
[@@deriving eq, show]

type event =
  [ `Created of create
  | `Details_updated of Entity.participant * update
  | `Password_updated of Entity.participant * Entity.Password.t
  | `Email_updated of Entity.participant * Entity.Email.t
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
