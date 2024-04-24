module User = Pool_user
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

let set_password pool { user; _ } password password_confirmation =
  let open Utils.Lwt_result.Infix in
  User.set_password pool user password password_confirmation >|+ ignore
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
  | Created
      { user_id
      ; email
      ; lastname
      ; firstname
      ; password
      ; terms_accepted_at
      ; language
      } ->
    let%lwt user =
      let id = user_id |> Id.to_user in
      User.create_user pool ~id email lastname firstname password
    in
    let contact = Entity.create ?terms_accepted_at ?language user in
    let%lwt () = Repo.insert pool contact in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) contact
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | EmailUpdated (contact, email) ->
    let%lwt _ = User.update pool ~email contact.user in
    Lwt.return_unit
  | PasswordUpdated (person, old_password, new_password, confirmed) ->
    let%lwt (_ : (Pool_user.t, Pool_message.Error.t) result) =
      User.update_password
        pool
        ~old_password
        ~new_password
        ~new_password_confirmation:confirmed
        person.user
      >|- Pool_common.Utils.with_log_error ~src ?tags
    in
    Lwt.return_unit
  | Verified contact ->
    Repo.update
      pool
      { contact with verified = Some (Pool_user.Verified.create_now ()) }
  | EmailVerified contact ->
    let%lwt _ =
      User.update pool Pool_user.{ contact.user with confirmed = true }
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
    let%lwt (_ : (Pool_user.t, Pool_message.Error.t) result) =
      User.set_password
        pool
        contact.user
        password
        (Pool_user.Password.to_confirmed password)
      >|- Pool_common.Utils.with_log_error ~src ?tags
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
