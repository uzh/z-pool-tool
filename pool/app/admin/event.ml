module Common = Common
open Entity

type creatable_admin =
  | Assistant
  | Experimenter
  | Recruiter
  | LocationManager
  | Operator
[@@deriving eq, show]

type create =
  { email : Common.Email.Address.t
  ; password : Common.Password.t
  ; firstname : Common.Firstname.t
  ; lastname : Common.Lastname.t
  }
[@@deriving eq, show]

type update =
  { firstname : Common.Firstname.t
  ; lastname : Common.Lastname.t
  }
[@@deriving eq, show]

type 'a person_event =
  | DetailsUpdated of 'a t * update
  | PasswordUpdated of 'a t * Common.Password.t * Common.PasswordConfirmed.t
  | Disabled of 'a t
  | Verified of 'a t

type event =
  | Created of creatable_admin * create
  | AssistantEvents of assistant person_event
  | ExperimenterEvents of experimenter person_event
  | LocationManagerEvents of location_manager person_event
  | RecruiterEvents of recruiter person_event
  | OperatorEvents of operator person_event

let handle_person_event : 'a person_event -> unit Lwt.t =
  let open Lwt.Syntax in
  function
  | DetailsUpdated (params, person) -> Repo.update person params
  | PasswordUpdated (person, password, confirmed) ->
    let* _ = Repo.set_password person password confirmed in
    Lwt.return_unit
  | Disabled _ -> Utils.todo ()
  | Verified _ -> Utils.todo ()
;;

let handle_event : event -> unit Lwt.t =
  let open Lwt.Syntax in
  function
  | Created (role, admin) ->
    let* user =
      Service.User.create_user
        ~name:admin.firstname
        ~given_name:admin.lastname
        ~password:admin.password
        (Common.Email.Address.show admin.email)
    in
    let* () =
      match role with
      | Assistant -> Permission.assign user (Role.assistant user.id)
      | Experimenter -> Permission.assign user (Role.experimenter user.id)
      | Recruiter -> Permission.assign user (Role.recruiter user.id)
      | LocationManager ->
        Permission.assign user (Role.location_manager user.id)
      | Operator -> Permission.assign user (Role.operator user.id)
    in
    Repo.insert user
  | AssistantEvents event -> handle_person_event event
  | ExperimenterEvents event -> handle_person_event event
  | LocationManagerEvents event -> handle_person_event event
  | RecruiterEvents event -> handle_person_event event
  | OperatorEvents event -> handle_person_event event
;;

let equal_person_event (one : 'a person_event) (two : 'a person_event) : bool =
  match one, two with
  | DetailsUpdated (p1, one), DetailsUpdated (p2, two) ->
    equal p1 p2 && equal_update one two
  | PasswordUpdated (p1, one, _), PasswordUpdated (p2, two, _) ->
    equal p1 p2 && Common.Password.equal one two
  | Disabled p1, Disabled p2 -> equal p1 p2
  | Verified p1, Verified p2 -> equal p1 p2
  | _ -> false
;;

let pp_person_event formatter (event : 'a person_event) : unit =
  let person_pp = pp formatter in
  match event with
  | DetailsUpdated (m, updated) ->
    let () = person_pp m in
    pp_update formatter updated
  | PasswordUpdated (m, _, _) ->
    let () = person_pp m in
    Common.Password.pp formatter "******"
  | Disabled m | Verified m -> person_pp m
;;

let equal_event event1 event2 : bool =
  match event1, event2 with
  | Created (role1, m), Created (role2, p) ->
    equal_creatable_admin role1 role2 && equal_create m p
  | AssistantEvents m, AssistantEvents p -> equal_person_event m p
  | ExperimenterEvents m, ExperimenterEvents p -> equal_person_event m p
  | LocationManagerEvents m, LocationManagerEvents p -> equal_person_event m p
  | RecruiterEvents m, RecruiterEvents p -> equal_person_event m p
  | OperatorEvents m, OperatorEvents p -> equal_person_event m p
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | Created (role, m) ->
    let () = pp_creatable_admin formatter role in
    pp_create formatter m
  | AssistantEvents m -> pp_person_event formatter m
  | ExperimenterEvents m -> pp_person_event formatter m
  | LocationManagerEvents m -> pp_person_event formatter m
  | RecruiterEvents m -> pp_person_event formatter m
  | OperatorEvents m -> pp_person_event formatter m
;;
