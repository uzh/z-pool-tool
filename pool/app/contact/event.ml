module User = Pool_user
module Id = Pool_common.Id
module Database = Pool_database
open Entity

type create =
  { user_id : Id.t
  ; email : User.EmailAddress.t
  ; password : User.Password.t [@opaque]
  ; firstname : User.Firstname.t
  ; lastname : User.Lastname.t
  ; terms_accepted_at : User.TermsAccepted.t option
  ; language : Pool_common.Language.t option
  }
[@@deriving eq, show]

type session_participation =
  { no_show : bool
  ; participated : bool
  }
[@@deriving eq, show]

let set_password
  : Database.Label.t -> t -> string -> string -> (unit, string) Lwt_result.t
  =
 fun pool { user; _ } password password_confirmation ->
  let open Utils.Lwt_result.Infix in
  Service.User.set_password
    ~ctx:(Pool_database.to_ctx pool)
    user
    ~password
    ~password_confirmation
  >|+ ignore
;;

let has_terms_accepted pool (contact : t) =
  let%lwt last_updated = Settings.terms_and_conditions_last_updated pool in
  let terms_accepted_at =
    contact.terms_accepted_at |> CCOption.map User.TermsAccepted.value
  in
  CCOption.map (Ptime.is_later ~than:last_updated) terms_accepted_at
  |> CCOption.get_or ~default:false
  |> Lwt.return
;;

type event =
  | Created of create
  | EmailUpdated of t * User.EmailAddress.t
  | PasswordUpdated of
      t * User.Password.t * User.Password.t * User.PasswordConfirmed.t
  | Verified of t
  | EmailVerified of t
  | TermsAccepted of t
  | Disabled of t
  | UnverifiedDeleted of t
  | ProfileUpdateTriggeredAtUpdated of t list
  | RegistrationAttemptNotificationSent of t
  | Updated of t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Pool_database.to_ctx pool in
  function
  | Created contact ->
    let%lwt user =
      Service.User.create_user
        ~ctx
        ~id:(contact.user_id |> Id.value)
        ~name:(contact.lastname |> User.Lastname.value)
        ~given_name:(contact.firstname |> User.Firstname.value)
        ~password:(contact.password |> User.Password.to_sihl)
      @@ User.EmailAddress.value contact.email
    in
    let contact =
      { user
      ; terms_accepted_at = contact.terms_accepted_at
      ; language = contact.language
      ; experiment_type_preference = None
      ; paused = User.Paused.create false
      ; disabled = User.Disabled.create false
      ; verified = None
      ; email_verified = None
      ; num_invitations = NumberOfInvitations.init
      ; num_assignments = NumberOfAssignments.init
      ; num_show_ups = NumberOfShowUps.init
      ; num_no_shows = NumberOfNoShows.init
      ; num_participations = NumberOfParticipations.init
      ; firstname_version = Pool_common.Version.create ()
      ; lastname_version = Pool_common.Version.create ()
      ; paused_version = Pool_common.Version.create ()
      ; language_version = Pool_common.Version.create ()
      ; experiment_type_preference_version = Pool_common.Version.create ()
      ; created_at = Ptime_clock.now ()
      ; updated_at = Ptime_clock.now ()
      }
    in
    let%lwt () = Repo.insert pool contact in
    Entity_guard.Target.to_authorizable ~ctx:(Pool_database.to_ctx pool) contact
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : [> `Contact ] Guard.Target.t) -> ()
  | EmailUpdated (contact, email) ->
    let%lwt _ =
      Service.User.update
        ~ctx
        ~email:(Pool_user.EmailAddress.value email)
        contact.user
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
        ~password_policy:(CCFun.const (CCResult.return ()))
        ~old_password
        ~new_password
        ~new_password_confirmation
        person.user
    in
    Lwt.return_unit
  | Verified contact ->
    Repo.update
      pool
      { contact with verified = Some (Pool_user.Verified.create_now ()) }
  | EmailVerified contact ->
    let%lwt _ =
      Service.User.update ~ctx Sihl_user.{ contact.user with confirmed = true }
    in
    Repo.update
      pool
      { contact with
        email_verified = Some (Pool_user.EmailVerified.create_now ())
      }
  | TermsAccepted contact ->
    Repo.update
      pool
      { contact with
        terms_accepted_at = Some (User.TermsAccepted.create_now ())
      }
  | Disabled contact ->
    Repo.update pool { contact with disabled = User.Disabled.create true }
  | UnverifiedDeleted contact ->
    contact |> Entity.id |> Repo.delete_unverified pool
  | ProfileUpdateTriggeredAtUpdated contacts ->
    contacts |> CCList.map id |> Repo.update_profile_updated_triggered pool
  | RegistrationAttemptNotificationSent t ->
    Repo.set_registration_attempt_notification_sent_at pool t
  | Updated contact -> contact |> Repo.update pool
;;
