open Common.Entity
open Entity

type create =
  { email : Email.unverified Email.t [@equal Email.equal] [@printer Email.pp]
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
  | EmailUpdated of t * Email.unverified Email.t
  | EmailVerify of Email.unverified Email.t
  | DetailsUpdated of t * update
  | PasswordUpdated of t * Password.t * PasswordConfirmed.t
  | Disabled of t
  | Verified of t

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
  | EmailVerify email ->
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
  | DetailsUpdated (params, person) -> Repo.update person params
  | PasswordUpdated (person, password, confirmed) ->
    let* _ = Repo.set_password person password confirmed in
    Lwt.return_unit
  | EmailUpdated (_, _) -> Utils.todo ()
  | Disabled _ -> Utils.todo ()
  | Verified _ -> Utils.todo ()
;;

let equal_event (one : event) (two : event) : bool =
  match one, two with
  | Created m, Created p -> equal_create m p
  | EmailVerify m, EmailVerify p -> Email.equal m p
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

let pp_person_event formatter (event : event) : unit =
  let person_pp = pp formatter in
  match event with
  | Created m -> pp_create formatter m
  | EmailVerify m -> Email.pp formatter m
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
