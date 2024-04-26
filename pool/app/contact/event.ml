open Entity

let src = Logs.Src.create "contact.event"

type create =
  { user_id : Id.t
  ; email : Pool_user.EmailAddress.t
  ; password : Pool_user.Password.Plain.t [@opaque]
  ; firstname : Pool_user.Firstname.t
  ; lastname : Pool_user.Lastname.t
  ; terms_accepted_at : Pool_user.TermsAccepted.t option
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
  Pool_user.Password.define
    pool
    user.Pool_user.id
    password
    password_confirmation
  >|+ ignore
;;

type event =
  | Created of create
  | EmailUpdated of t * Pool_user.EmailAddress.t
  | Verified of t
  | EmailVerified of t
  | TermsAccepted of t
  | Disabled of t
  | UnverifiedDeleted of t
  | CellPhoneAdded of t * Pool_user.CellPhone.t * Pool_common.VerificationCode.t
  | CellPhoneVerified of t * Pool_user.CellPhone.t
  | CellPhoneVerificationReset of t
  | ImportConfirmed of t * Pool_user.Password.Plain.t
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
      let open Pool_common.Utils in
      let id = user_id |> Id.to_user in
      Pool_user.create_user
        pool
        ~id
        email
        lastname
        firstname
        password
        (Pool_user.Password.to_confirmed password)
      >|- with_log_error ~src ?tags
      ||> get_or_failwith
    in
    let contact = Entity.create ?terms_accepted_at ?language user in
    let%lwt () = Repo.insert pool contact in
    Entity_guard.Target.to_authorizable ~ctx:(Database.to_ctx pool) contact
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | EmailUpdated (contact, email) ->
    let%lwt _ = Pool_user.update pool ~email contact.user in
    Lwt.return_unit
  | Verified contact ->
    Repo.update
      pool
      { contact with verified = Some (Pool_user.Verified.create_now ()) }
  | EmailVerified contact ->
    let%lwt (_ : Pool_user.t) = contact |> user |> Pool_user.confirm pool in
    Repo.update
      pool
      { contact with
        email_verified = Some (Pool_user.EmailVerified.create_now ())
      }
  | TermsAccepted contact ->
    Repo.update
      pool
      { contact with
        terms_accepted_at = Some (Pool_user.TermsAccepted.create_now ())
      }
  | Disabled contact ->
    Repo.update pool { contact with disabled = Pool_user.Disabled.create true }
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
    let%lwt (_ : (unit, Pool_message.Error.t) result) =
      Pool_user.Password.define
        pool
        (contact |> user |> Pool_user.id)
        password
        (Pool_user.Password.to_confirmed password)
      >|- Pool_common.Utils.with_log_error ~src ?tags
    in
    Repo.update
      pool
      { contact with
        import_pending = Pool_user.ImportPending.create false
      ; terms_accepted_at = Some (Pool_user.TermsAccepted.create_now ())
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
