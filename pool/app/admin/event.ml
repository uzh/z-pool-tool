open Common.Entity
open Entity

type creatable_admin =
  | Assistant
  | Experimenter
  | Recruiter
  | LocationManager
  | Operator
[@@deriving eq, show]

type create =
  { role : creatable_admin
  ; email : Email.unverified Email.t [@equal Email.equal] [@printer Email.pp]
  ; password : Password.t
  ; firstname : Firstname.t
  ; lastname : Lastname.t
  }
[@@deriving eq, show]

type update =
  { firstname : Firstname.t
  ; lastname : Lastname.t
  }
[@@deriving eq, show]

type update_email =
  { email : Email.unverified Email.t [@equal Email.equal] [@printer Email.pp] }
[@@deriving eq, show]

type 'a person_event =
  | DetailsUpdated of 'a t * update
  | PasswordUpdated of 'a t * Password.t * PasswordConfirmed.t
  | EmailUpdated of 'a t * Email.unverified Email.t
  | Disabled of 'a t
  | Verified of 'a t

type event =
  | Created of create
  | VerifyEmail of update_email
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
  | EmailUpdated (_, _) -> Utils.todo ()
  | Disabled _ -> Utils.todo ()
  | Verified _ -> Utils.todo ()
;;

let handle_event : event -> unit Lwt.t =
  let open Lwt.Syntax in
  function
  | Created participant ->
    let email_address = participant.email |> Email.show in
    let* user =
      Service.User.create_user
        ~name:participant.firstname
        ~given_name:participant.lastname
        ~password:participant.password
        email_address
    in
    let* () = Permission.assign user (Role.participant user.id) in
    Repo.insert participant
  | VerifyEmail { email } ->
    let* _ =
      let open Lwt.Syntax in
      let* user = Service.User.find_by_email_opt (email |> Email.show) in
      match user with
      | Some user ->
        let _ = Service.User.update { user with confirmed = true } in
        Lwt.return_unit
      | None ->
        Logs.warn (fun m -> m "Could not be verified!");
        Lwt.return_unit
    in
    let () = Utils.todo email in
    Lwt.return_unit
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
    equal p1 p2 && Password.equal one two
  | EmailUpdated (p1, one), EmailUpdated (p2, two) ->
    equal p1 p2 && Email.equal one two
  | Disabled p1, Disabled p2 -> equal p1 p2
  | Verified p1, Verified p2 -> equal p1 p2
  | _ -> false
;;

let pp_person_event formatter (event : 'a person_event) : unit =
  let person_pp = pp formatter in
  match event with
  | DetailsUpdated (p1, updated) ->
    let () = person_pp p1 in
    pp_update formatter updated
  | PasswordUpdated (p1, _, _) ->
    let () = person_pp p1 in
    Password.pp formatter "******"
  | EmailUpdated (p1, updated) ->
    let () = person_pp p1 in
    Email.pp formatter updated
  | Disabled p1 | Verified p1 -> person_pp p1
;;

let equal_event event1 event2 : bool =
  match event1, event2 with
  | Created m, Created p -> equal_create m p
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
  | VerifyEmail m -> pp_update_email formatter m
  | AssistantEvents m -> pp_person_event formatter m
  | ExperimenterEvents m -> pp_person_event formatter m
  | LocationManagerEvents m -> pp_person_event formatter m
  | RecruiterEvents m -> pp_person_event formatter m
  | OperatorEvents m -> pp_person_event formatter m
;;
