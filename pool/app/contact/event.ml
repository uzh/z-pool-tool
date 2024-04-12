module User = Pool_user
module Id = Pool_common.Id
open Entity

let src = Logs.Src.create "contact.event"

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
  User.set_password pool user ~password ~password_confirmation >|+ ignore
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
  | CellPhoneAdded of t * User.CellPhone.t * Pool_common.VerificationCode.t
  | CellPhoneVerified of t * User.CellPhone.t
  | CellPhoneVerificationReset of t
  | ImportConfirmed of t * User.Password.t
  | ImportDisabled of t
  | ProfileUpdateTriggeredAtUpdated of t list
  | RegistrationAttemptNotificationSent of t
  | Updated of t
  | SignInCounterUpdated of t
[@@deriving eq, show, variants]

let handle_event ?tags pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  function
  | Created contact ->
    let%lwt user =
      User.create_user
        pool
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
      ; cell_phone = None
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
      ; import_pending = Pool_user.ImportPending.create false
      ; created_at = Ptime_clock.now ()
      ; updated_at = Ptime_clock.now ()
      }
    in
    let%lwt () = Repo.insert pool contact in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) contact
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | EmailUpdated (contact, email) ->
    let%lwt _ =
      User.update pool ~email:(Pool_user.EmailAddress.value email) contact.user
    in
    Lwt.return_unit
  | PasswordUpdated (person, old_password, new_password, confirmed) ->
    let old_password = old_password |> User.Password.to_sihl in
    let new_password = new_password |> User.Password.to_sihl in
    let new_password_confirmation =
      confirmed |> User.PasswordConfirmed.to_sihl
    in
    let%lwt _ =
      User.update_password
        pool
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
      User.update pool Sihl_user.{ contact.user with confirmed = true }
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
  | CellPhoneAdded (contact, cell_phone, token) ->
    Repo.add_cell_phone pool contact cell_phone token
  | CellPhoneVerified (contact, cell_phone) ->
    let%lwt () =
      { contact with cell_phone = Some cell_phone } |> Repo.update pool
    in
    Repo.delete_unverified_cell_phone pool contact
  | CellPhoneVerificationReset contact ->
    Repo.delete_unverified_cell_phone pool contact
  | ImportConfirmed (contact, password) ->
    let (_ : (Sihl_user.t Lwt.t, string) result) =
      let open Pool_common in
      User.set_user_password contact.user (User.Password.to_sihl password)
      |> CCResult.map (User.update pool)
      |> Utils.with_log_result_error ~src ?tags Pool_message.Error.nothandled
    in
    Repo.update
      pool
      { contact with
        import_pending = Pool_user.ImportPending.create false
      ; terms_accepted_at = Some (User.TermsAccepted.create_now ())
      }
  | ImportDisabled contact ->
    Repo.update
      pool
      { contact with import_pending = Pool_user.ImportPending.create false }
  | ProfileUpdateTriggeredAtUpdated contacts ->
    contacts |> CCList.map id |> Repo.update_profile_updated_triggered pool
  | RegistrationAttemptNotificationSent t ->
    Repo.set_registration_attempt_notification_sent_at pool t
  | Updated contact -> contact |> Repo.update pool
  | SignInCounterUpdated contact -> Repo.update_sign_in_count pool contact
;;
