module User = Common_user
module Common = Pool_common
open Entity

type creatable_admin =
  | Assistant
  | Experimenter
  | Recruiter
  | LocationManager
  | Operator
  | Root
[@@deriving eq, show]

type create =
  { email : User.Email.Address.t
  ; password : User.Password.t
  ; firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  }
[@@deriving eq, show]

type update =
  { firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  }
[@@deriving eq, show]

type 'a person_event =
  | DetailsUpdated of 'a t * update
  | PasswordUpdated of 'a t * User.Password.t * User.PasswordConfirmed.t
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
    let* _ =
      Repo.set_password
        person
        (password |> User.Password.to_sihl)
        (confirmed |> User.PasswordConfirmed.to_sihl)
    in
    Lwt.return_unit
  | Disabled _ -> Utils.todo ()
  | Verified _ -> Utils.todo ()
;;

let[@warning "-4"] handle_event : event -> unit Lwt.t =
  let open Lwt.Syntax in
  function
  | Created (role, admin) ->
    let* user =
      Service.User.create_user
        ~name:(admin.lastname |> User.Lastname.value)
        ~given_name:(admin.firstname |> User.Firstname.value)
        ~password:(admin.password |> User.Password.to_sihl)
        (User.Email.Address.value admin.email)
    in
    let person =
      { user
      ; created_at = Common.CreatedAt.create ()
      ; updated_at = Common.UpdatedAt.create ()
      }
    in
    let* () =
      match role with
      | Assistant ->
        Permission.assign
          user
          (Role.assistant (user.Sihl_user.id |> Common.Id.of_string))
      | Experimenter ->
        Permission.assign
          user
          (Role.experimenter (user.Sihl_user.id |> Common.Id.of_string))
      | Recruiter ->
        Permission.assign
          user
          (Role.recruiter (user.Sihl_user.id |> Common.Id.of_string))
      | LocationManager ->
        Permission.assign
          user
          (Role.location_manager (user.Sihl_user.id |> Common.Id.of_string))
      | Operator ->
        let* _ = Repo.insert (Operator person) in
        Permission.assign
          user
          (Role.operator (user.Sihl_user.id |> Common.Id.of_string))
      | Root -> Permission.assign user Role.root
    in
    Lwt.return_unit
  | AssistantEvents event -> handle_person_event event
  | ExperimenterEvents event -> handle_person_event event
  | LocationManagerEvents event -> handle_person_event event
  | RecruiterEvents event -> handle_person_event event
  | OperatorEvents event -> handle_person_event event
;;

let[@warning "-4"] equal_person_event
    (one : 'a person_event)
    (two : 'a person_event)
    : bool
  =
  match one, two with
  | DetailsUpdated (p1, one), DetailsUpdated (p2, two) ->
    equal p1 p2 && equal_update one two
  | PasswordUpdated (p1, one, _), PasswordUpdated (p2, two, _) ->
    equal p1 p2 && User.Password.equal one two
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
  | PasswordUpdated (m, p, _) ->
    let () = person_pp m in
    User.Password.pp formatter p
  | Disabled m | Verified m -> person_pp m
;;

let[@warning "-4"] equal_event event1 event2 : bool =
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
