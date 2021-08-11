open Entity

type create =
  { email : Email.t
  ; password : Password.t
  ; firstname : Firstname.t
  ; lastname : Lastname.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Sihl.timestamp
  }
[@@deriving eq, show]

type update =
  { email : Email.t
  ; firstname : Firstname.t
  ; lastname : Lastname.t
  ; paused : Paused.t
  }
[@@deriving eq, show]

type 'a person_event =
  | DetailsUpdated of 'a t * update
  | PasswordUpdated of 'a t * Password.t
  | EmailUpdated of 'a t * Email.t
  | Disabled of 'a t
  | Verified of 'a t

let equal_person_event (one : 'a person_event) (two : 'a person_event) : bool =
  match one, two with
  | DetailsUpdated (p1, one), DetailsUpdated (p2, two) ->
    Entity.equal p1 p2 && equal_update one two
  | _ -> false
;;

let pp_person_event formatter (event : 'a person_event) : unit =
  match event with
  | DetailsUpdated (p1, _)
  | PasswordUpdated (p1, _)
  | EmailUpdated (p1, _)
  | Disabled p1
  | Verified p1 -> Entity.pp formatter p1
;;

type event =
  | Created of create
  | ParticipantEvents of participant person_event
  | AssistantEvents of participant person_event
  | ExperimenterEvents of participant person_event

let equal_event event1 event2 : bool =
  match event1, event2 with
  | Created m, Created p -> equal_create m p
  | ParticipantEvents m, ParticipantEvents p
  | AssistantEvents m, AssistantEvents p
  | ExperimenterEvents m, ExperimenterEvents p -> equal_person_event m p
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | Created m -> pp_create formatter m
  | ParticipantEvents m | AssistantEvents m | ExperimenterEvents m ->
    pp_person_event formatter m
;;

let handle_event : event -> unit Lwt.t =
  let open Lwt.Syntax in
  function
  | Created participant ->
    let* user =
      Sihl.User.create ~email:participant.email ~password:participant.password
    in
    let* () = Permission.assign user (Role.participant user.id) in
    Repo.insert participant
  | ParticipantEvents event ->
    (match event with
    | DetailsUpdated _ as user -> Repo.update user
    | PasswordUpdated (Participant person, password) ->
      Sihl.User.set_password person.user password
    | EmailUpdated (_, _) -> Sihl.todo ()
    | Disabled _ -> Sihl.todo ()
    | Verified _ -> Sihl.todo ())
  | _ -> Lwt.return_unit
;;
