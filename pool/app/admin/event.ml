module User = Pool_user
module Common = Pool_common
module Database = Pool_database
open Entity

type role =
  | Assistant
  | Experimenter
  | Recruiter
  | LocationManager
  | Operator
[@@deriving eq, show]

type create =
  { email : User.EmailAddress.t
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

let set_password
    : type person.
      Database.Label.t
      -> person t
      -> string
      -> string
      -> (unit, string) result Lwt.t
  =
 fun pool person password password_confirmation ->
  let open Lwt_result.Infix in
  match person with
  | Assistant { user; _ }
  | Experimenter { user; _ }
  | LocationManager { user; _ }
  | Recruiter { user; _ }
  | Operator { user; _ } ->
    Service.User.set_password
      ~ctx:(Pool_tenant.to_ctx pool)
      user
      ~password
      ~password_confirmation
    >|= ignore
;;

type 'a person_event =
  | DetailsUpdated of 'a t * update
  | PasswordUpdated of 'a t * User.Password.t * User.PasswordConfirmed.t
  | RoleUpdated of 'a t * role
  | Disabled of 'a t
  | Verified of 'a t

type event =
  | Created of role * create
  | AssistantEvents of assistant person_event
  | ExperimenterEvents of experimenter person_event
  | LocationManagerEvents of location_manager person_event
  | RecruiterEvents of recruiter person_event
  | OperatorEvents of operator person_event

let handle_person_event pool : 'a person_event -> unit Lwt.t = function
  | DetailsUpdated (_, _) -> Lwt.return_unit
  | PasswordUpdated (person, password, confirmed) ->
    (* TODO [aerben] this can fail, why is it not caught? *)
    (* TODO [aerben] use update_password? *)
    let%lwt _ =
      set_password
        pool
        person
        (password |> User.Password.to_sihl)
        (confirmed |> User.PasswordConfirmed.to_sihl)
    in
    Lwt.return_unit
  | RoleUpdated _ -> Utils.todo ()
  | Disabled _ -> Utils.todo ()
  | Verified _ -> Utils.todo ()
;;

let handle_event pool = function
  | Created (role, admin) ->
    let%lwt user =
      Service.User.create_admin
        ~ctx:(Pool_tenant.to_ctx pool)
        ~name:(admin.lastname |> User.Lastname.value)
        ~given_name:(admin.firstname |> User.Firstname.value)
        ~password:(admin.password |> User.Password.to_sihl)
        (User.EmailAddress.value admin.email)
    in
    let person =
      { user
      ; created_at = Common.CreatedAt.create ()
      ; updated_at = Common.UpdatedAt.create ()
      }
    in
    let open Lwt.Infix in
    let%lwt fn =
      match role with
      | Assistant ->
        Repo.insert pool (Assistant person) >|= fun _ -> Role.assistant
      | Experimenter ->
        Repo.insert pool (Experimenter person) >|= fun _ -> Role.experimenter
      | Recruiter ->
        Repo.insert pool (Recruiter person) >|= fun _ -> Role.recruiter
      | LocationManager ->
        Repo.insert pool (LocationManager person)
        >|= fun _ -> Role.location_manager
      | Operator ->
        Repo.insert pool (Operator person) >|= fun _ -> Role.operator
    in
    Permission.assign user (fn (user.Sihl_user.id |> Common.Id.of_string))
  | AssistantEvents event -> handle_person_event pool event
  | ExperimenterEvents event -> handle_person_event pool event
  | LocationManagerEvents event -> handle_person_event pool event
  | RecruiterEvents event -> handle_person_event pool event
  | OperatorEvents event -> handle_person_event pool event
;;

(* TODO [aerben] do this everywhere where warning -4 suppressed, also rename
   args to make sense*)
let equal_person_event event1 event2 =
  match event1, event2 with
  | DetailsUpdated (p1, one), DetailsUpdated (p2, two) ->
    equal p1 p2 && equal_update one two
  | PasswordUpdated (p1, one, _), PasswordUpdated (p2, two, _) ->
    equal p1 p2 && User.Password.equal one two
  | RoleUpdated (p1, a1), RoleUpdated (p2, a2) ->
    equal p1 p2 && equal_role a1 a2
  | Disabled p1, Disabled p2 | Verified p1, Verified p2 -> equal p1 p2
  (* Match the rest for exhaustiveness *)
  | DetailsUpdated _, _
  | PasswordUpdated _, _
  | RoleUpdated _, _
  | Disabled _, _
  | Verified _, _ -> false
;;

let pp_person_event formatter (event : 'a person_event) : unit =
  let person_pp = pp formatter in
  match event with
  | DetailsUpdated (m, updated) ->
    person_pp m;
    pp_update formatter updated
  | PasswordUpdated (m, p, _) ->
    person_pp m;
    User.Password.pp formatter p
  | RoleUpdated (m, a) ->
    person_pp m;
    pp_role formatter a
  | Disabled m | Verified m -> person_pp m
;;

let[@warning "-4"] equal_event event1 event2 : bool =
  match event1, event2 with
  | Created (role1, m), Created (role2, p) ->
    equal_role role1 role2 && equal_create m p
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
    pp_role formatter role;
    pp_create formatter m
  | AssistantEvents m -> pp_person_event formatter m
  | ExperimenterEvents m -> pp_person_event formatter m
  | LocationManagerEvents m -> pp_person_event formatter m
  | RecruiterEvents m -> pp_person_event formatter m
  | OperatorEvents m -> pp_person_event formatter m
;;
