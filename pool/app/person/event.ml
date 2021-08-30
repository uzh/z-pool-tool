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
  | PasswordUpdated (p1, one), PasswordUpdated (p2, two) ->
    Entity.equal p1 p2 && Password.equal one two
  | EmailUpdated (p1, one), EmailUpdated (p2, two) ->
    Entity.equal p1 p2 && Email.equal one two
  | Disabled p1, Disabled p2 -> Entity.equal p1 p2
  | Verified p1, Verified p2 -> Entity.equal p1 p2
  | _ -> false
;;

let pp_person_event formatter (event : 'a person_event) : unit =
  let person_pp = Entity.pp formatter in
  match event with
  | DetailsUpdated (p1, updated) ->
    let () = person_pp p1 in
    pp_update formatter updated
  | PasswordUpdated (p1, updated) ->
    let () = person_pp p1 in
    Entity.Password.pp formatter updated
  | EmailUpdated (p1, updated) ->
    let () = person_pp p1 in
    Entity.Email.pp formatter updated
  | Disabled p1 | Verified p1 -> person_pp p1
;;

type event =
  | Created of create
  | ParticipantEvents of participant person_event
  | AssistantEvents of assistant person_event
  | ExperimenterEvents of experimenter person_event
  | LocationManagerEvents of location_manager person_event
  | RecruiterEvents of recruiter person_event
  | OperatorEvents of operator person_event

let equal_event event1 event2 : bool =
  match event1, event2 with
  | Created m, Created p -> equal_create m p
  | ParticipantEvents m, ParticipantEvents p -> equal_person_event m p
  | AssistantEvents m, AssistantEvents p -> equal_person_event m p
  | ExperimenterEvents m, ExperimenterEvents p -> equal_person_event m p
  | LocationManagerEvents m, LocationManagerEvents p -> equal_person_event m p
  | RecruiterEvents m, RecruiterEvents p -> equal_person_event m p
  | OperatorEvents m, OperatorEvents p -> equal_person_event m p
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | Created m -> pp_create formatter m
  | ParticipantEvents m -> pp_person_event formatter m
  | AssistantEvents m -> pp_person_event formatter m
  | ExperimenterEvents m -> pp_person_event formatter m
  | LocationManagerEvents m -> pp_person_event formatter m
  | RecruiterEvents m -> pp_person_event formatter m
  | OperatorEvents m -> pp_person_event formatter m
;;

let handle_person_event : 'a person_event -> unit Lwt.t = function
  | DetailsUpdated _ as user -> Repo.update user
  | PasswordUpdated (person, password) ->
    (* Unpack Sihl.User from variant *)
    Sihl.User.set_password person password
  | EmailUpdated (_, _) -> Sihl.todo ()
  | Disabled _ -> Sihl.todo ()
  | Verified _ -> Sihl.todo ()
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
  | ParticipantEvents event -> handle_person_event event
  | AssistantEvents event -> handle_person_event event
  | ExperimenterEvents event -> handle_person_event event
  | LocationManagerEvents event -> handle_person_event event
  | RecruiterEvents event -> handle_person_event event
  | OperatorEvents event -> handle_person_event event
;;
