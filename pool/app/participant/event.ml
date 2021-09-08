open Common.Entity
open Entity

type create =
  { email : Email.Address.t
  ; password : Password.t
  ; firstname : Firstname.t
  ; lastname : Lastname.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : TermsAccepted.t
  }
[@@deriving eq, show]

type update =
  { firstname : Firstname.t
  ; lastname : Lastname.t
  ; paused : Paused.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | DetailsUpdated of t * update
  | PasswordUpdated of t * Password.t * PasswordConfirmed.t
  | Disabled of t
  | Verified of t

let handle_event : event -> unit Lwt.t =
  let open Lwt.Syntax in
  function
  | Created participant ->
    let* user =
      Service.User.create_user
        ~name:participant.firstname
        ~given_name:participant.lastname
        ~password:participant.password
      @@ Email.Address.show participant.email
    in
    let* () = Permission.assign user (Role.participant user.id) in
    Repo.insert participant
  | DetailsUpdated (params, person) -> Repo.update person params
  | PasswordUpdated (person, password, confirmed) ->
    let* _ = Repo.set_password person password confirmed in
    Lwt.return_unit
  | Disabled _ -> Utils.todo ()
  | Verified _ -> Utils.todo ()
;;

let equal_event (one : event) (two : event) : bool =
  match one, two with
  | Created m, Created p -> equal_create m p
  | DetailsUpdated (p1, one), DetailsUpdated (p2, two) ->
    equal p1 p2 && equal_update one two
  | PasswordUpdated (p1, one, _), PasswordUpdated (p2, two, _) ->
    equal p1 p2 && Password.equal one two
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
  | PasswordUpdated (p1, _, _) ->
    let () = person_pp p1 in
    Password.pp formatter "******"
  | Disabled p1 | Verified p1 -> person_pp p1
;;
