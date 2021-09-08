module Common = Common
open Entity

type create =
  { email : Email.Address.t
  ; password : Common.Password.t
  ; firstname : Common.Firstname.t
  ; lastname : Common.Lastname.t
  ; recruitment_channel : RecruitmentChannel.t
  ; terms_accepted_at : Common.TermsAccepted.t
  }
[@@deriving eq, show]

type update =
  { firstname : Common.Firstname.t
  ; lastname : Common.Lastname.t
  ; paused : Common.Paused.t
  }
[@@deriving eq, show]

type event =
  | Created of create
  | DetailsUpdated of t * update
  | PasswordUpdated of t * Common.Password.t * Common.PasswordConfirmed.t
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
    equal p1 p2 && Common.Password.equal one two
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
    Common.Password.pp formatter password
  | Disabled p1 | Verified p1 -> person_pp p1
;;
