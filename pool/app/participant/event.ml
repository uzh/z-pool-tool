module User = Common_user
module Id = Common.Id
open Entity

type create =
  { email : Email.Address.t
  ; password : User.Password.t
  ; firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : User.TermsAccepted.t
  }
[@@deriving eq, show]

type update =
  { firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  ; paused : User.Paused.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | DetailsUpdated of t * update
  | PasswordUpdated of t * User.Password.t * User.PasswordConfirmed.t
  | Disabled of t
  | Verified of t
  | Email of User.Event.Email.event

let handle_event : event -> unit Lwt.t =
  let open Lwt.Syntax in
  function
  | Created participant ->
    let* user =
      Service.User.create_user
        ~name:(participant.firstname |> User.Firstname.show)
        ~given_name:(participant.lastname |> User.Lastname.show)
        ~password:(participant.password |> User.Password.to_sihl)
      @@ Email.Address.show participant.email
    in
    let* () =
      Permission.assign user (Role.participant (user.id |> Id.of_string))
    in
    Repo.insert participant
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
  | Email event -> User.Event.Email.handle_event event
;;

let equal_event (one : event) (two : event) : bool =
  match one, two with
  | Created m, Created p -> equal_create m p
  | DetailsUpdated (p1, one), DetailsUpdated (p2, two) ->
    equal p1 p2 && equal_update one two
  | PasswordUpdated (p1, one, _), PasswordUpdated (p2, two, _) ->
    equal p1 p2 && User.Password.equal one two
  | Disabled p1, Disabled p2 -> equal p1 p2
  | Verified p1, Verified p2 -> equal p1 p2
  | _ -> false
;;

let pp_event formatter (event : event) : unit =
  let person_pp = pp formatter in
  match event with
  | Created m -> pp_create formatter m
  | DetailsUpdated (p1, updated) ->
    let () = person_pp p1 in
    pp_update formatter updated
  | PasswordUpdated (person, password, _) ->
    let () = person_pp person in
    User.Password.pp formatter password
  | Disabled p1 | Verified p1 -> person_pp p1
  | Email m -> User.Event.Email.pp_event formatter m
;;
