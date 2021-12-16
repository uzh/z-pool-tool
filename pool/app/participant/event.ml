module User = Pool_user
module Id = Pool_common.Id
module Database = Pool_database
open Entity

type create =
  { user_id : Id.t
  ; email : User.EmailAddress.t
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

let set_password
    : Database.Label.t -> t -> string -> string -> (unit, string) result Lwt.t
  =
 fun pool { user; _ } password password_confirmation ->
  let open Lwt_result.Infix in
  Service.User.set_password
    ~ctx:(Pool_tenant.to_ctx pool)
    user
    ~password
    ~password_confirmation
  >|= ignore
;;

let send_password_changed_email pool email firstname lastname =
  let open Lwt.Infix in
  Email.Helper.PasswordChange.create pool email firstname lastname
  >>= Service.Email.send ~ctx:(Pool_tenant.to_ctx pool)
;;

let has_terms_accepted pool (participant : t) =
  let%lwt last_updated = Settings.terms_and_conditions_last_updated pool in
  let terms_accepted_at =
    participant.terms_accepted_at |> User.TermsAccepted.value
  in
  CCOption.map (Ptime.is_later ~than:last_updated) terms_accepted_at
  |> CCOption.get_or ~default:false
  |> Lwt.return
;;

type event =
  | Created of create
  | FirstnameUpdated of t * User.Firstname.t
  | LastnameUpdated of t * User.Lastname.t
  | PausedUpdated of t * User.Paused.t
  | EmailUpdated of t * User.EmailAddress.t
  | PasswordUpdated of
      t * User.Password.t * User.Password.t * User.PasswordConfirmed.t
  | EmailConfirmed of t
  | TermsAccepted of t
  | Disabled of t
  | Verified of t
[@@deriving variants]

let handle_event pool : event -> unit Lwt.t =
  let ctx = Pool_tenant.to_ctx pool in
  function
  | Created participant ->
    let%lwt user =
      Service.User.create_user
        ~ctx
        ~id:(participant.user_id |> Id.value)
        ~name:(participant.lastname |> User.Lastname.value)
        ~given_name:(participant.firstname |> User.Firstname.value)
        ~password:(participant.password |> User.Password.to_sihl)
      @@ User.EmailAddress.value participant.email
    in
    { user
    ; recruitment_channel = participant.recruitment_channel
    ; terms_accepted_at = participant.terms_accepted_at
    ; paused = User.Paused.create false
    ; disabled = User.Disabled.create false
    ; verified = User.Verified.create None
    ; firstname_version = Pool_common.Version.create ()
    ; lastname_version = Pool_common.Version.create ()
    ; paused_version = Pool_common.Version.create ()
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
    |> Repo.insert pool
    |> CCFun.const Lwt.return_unit
  | FirstnameUpdated (participant, firstname) ->
    let%lwt _ =
      Service.User.update
        ~ctx
        ~given_name:(firstname |> User.Firstname.value)
        participant.user
    in
    Repo.update_version_for
      pool
      `Firstname
      ( id participant
      , Pool_common.Version.increment participant.firstname_version )
  | LastnameUpdated (participant, lastname) ->
    let%lwt _ =
      Service.User.update
        ~ctx
        ~name:(lastname |> User.Lastname.value)
        participant.user
    in
    Repo.update_version_for
      pool
      `Lastname
      ( id participant
      , Pool_common.Version.increment participant.lastname_version )
  | PausedUpdated (participant, paused) ->
    let%lwt () =
      Repo.update_paused
        pool
        { participant with
          paused
        ; paused_version =
            Pool_common.Version.increment participant.paused_version
        }
    in
    Lwt.return_unit
  | EmailUpdated (participant, email) ->
    let%lwt _ =
      Service.User.update
        ~ctx
        ~email:(Pool_user.EmailAddress.value email)
        participant.user
    in
    Lwt.return_unit
  | PasswordUpdated (person, old_password, new_password, confirmed) ->
    let old_password = old_password |> User.Password.to_sihl in
    let new_password = new_password |> User.Password.to_sihl in
    let new_password_confirmation =
      confirmed |> User.PasswordConfirmed.to_sihl
    in
    let%lwt _ =
      Service.User.update_password
        ~ctx
        ~old_password
        ~new_password
        ~new_password_confirmation
        person.user
    in
    let%lwt email =
      let open Lwt.Infix in
      Email.find_verified_by_user
        pool
        (person.user.Sihl.Contract.User.id |> Id.of_string)
      >|= function
      | Ok email -> email
      | Error err ->
        Logs.err (fun m ->
            m "%s" Pool_common.(Utils.error_to_string Language.En err));
        failwith ""
    in
    let%lwt () =
      send_password_changed_email
        pool
        email
        (firstname person)
        (lastname person)
    in
    Lwt.return_unit
  | EmailConfirmed participant ->
    let%lwt _ =
      Service.User.update
        ~ctx
        Sihl_user.{ participant.user with confirmed = true }
    in
    Lwt.return_unit
  | TermsAccepted participant ->
    let%lwt () =
      Repo.update
        pool
        { participant with
          terms_accepted_at = User.TermsAccepted.create_now ()
        }
    in
    Lwt.return_unit
  | Disabled _ -> Utils.todo ()
  | Verified _ -> Utils.todo ()
;;

let[@warning "-4"] equal_event (one : event) (two : event) : bool =
  match one, two with
  | Created m, Created p -> equal_create m p
  | FirstnameUpdated (p1, one), FirstnameUpdated (p2, two) ->
    equal p1 p2 && User.Firstname.equal one two
  | LastnameUpdated (p1, one), LastnameUpdated (p2, two) ->
    equal p1 p2 && User.Lastname.equal one two
  | PausedUpdated (p1, one), PausedUpdated (p2, two) ->
    equal p1 p2 && User.Paused.equal one two
  | EmailUpdated (p1, e1), EmailUpdated (p2, e2) ->
    equal p1 p2 && Pool_user.EmailAddress.equal e1 e2
  | PasswordUpdated (p1, old1, new1, _), PasswordUpdated (p2, old2, new2, _) ->
    equal p1 p2
    && User.Password.equal old1 old2
    && User.Password.equal new1 new2
  | TermsAccepted p1, TermsAccepted p2 -> equal p1 p2
  | Disabled p1, Disabled p2 -> equal p1 p2
  | Verified p1, Verified p2 -> equal p1 p2
  | _ -> false
;;

let pp_event formatter (event : event) : unit =
  let person_pp = pp formatter in
  match event with
  | Created m -> pp_create formatter m
  | FirstnameUpdated (p, m) ->
    person_pp p;
    User.Firstname.pp formatter m
  | LastnameUpdated (p, m) ->
    person_pp p;
    User.Lastname.pp formatter m
  | PausedUpdated (p, m) ->
    person_pp p;
    User.Paused.pp formatter m
  | EmailUpdated (p, m) ->
    person_pp p;
    Pool_user.EmailAddress.pp formatter m
  | PasswordUpdated (person, _, password, _) ->
    person_pp person;
    User.Password.pp formatter password
  | EmailConfirmed p | TermsAccepted p | Disabled p | Verified p -> person_pp p
;;
