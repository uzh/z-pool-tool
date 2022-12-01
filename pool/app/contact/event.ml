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
  ; recruitment_channel : RecruitmentChannel.t option
  ; terms_accepted_at : User.TermsAccepted.t option
  ; language : Pool_common.Language.t option
  }
[@@deriving eq, show]

type session_participation =
  { show_up : bool
  ; participated : bool
  }
[@@deriving eq, show]

let set_password
  : Database.Label.t -> t -> string -> string -> (unit, string) Lwt_result.t
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
  | Updated of PartialUpdate.t * t
  | EmailUpdated of t * User.EmailAddress.t
  | PasswordUpdated of
      t * User.Password.t * User.Password.t * User.PasswordConfirmed.t
  | Verified of t
  | EmailVerified of t
  | TermsAccepted of t
  | Disabled of t
  | UnverifiedDeleted of t
  (* TODO[timhub]: Actually implement those events, on invite / assignment (Do
     not forget to undo, when entity gets deleted) *)
  | NumAssignmentsDecreased of t
  | NumAssignmentsIncreased of t
  | NumInvitationsIncreased of t
  | ProfileUpdateTriggeredAtUpdated of t list
  | SessionParticipationSet of t * session_participation
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t =
  let ctx = Pool_tenant.to_ctx pool in
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
    { user
    ; recruitment_channel = contact.recruitment_channel
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
    ; num_participations = NumberOfParticipations.init
    ; firstname_version = Pool_common.Version.create ()
    ; lastname_version = Pool_common.Version.create ()
    ; paused_version = Pool_common.Version.create ()
    ; language_version = Pool_common.Version.create ()
    ; experiment_type_preference_version = Pool_common.Version.create ()
    ; created_at = Ptime_clock.now ()
    ; updated_at = Ptime_clock.now ()
    }
    |> Repo.insert pool
    |> CCFun.const Lwt.return_unit
  | Updated (update, contact) -> Repo.partial_update pool update contact
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
        ~password_policy:(CCFun.const (CCResult.pure ()))
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
  | NumAssignmentsDecreased ({ num_assignments; _ } as contact) ->
    Repo.update
      pool
      { contact with
        num_assignments = num_assignments |> NumberOfAssignments.decrement
      }
  | NumAssignmentsIncreased ({ num_assignments; _ } as contact) ->
    Repo.update
      pool
      { contact with
        num_assignments = num_assignments |> NumberOfAssignments.increment
      }
  | NumInvitationsIncreased ({ num_invitations; _ } as contact) ->
    Repo.update
      pool
      { contact with
        num_invitations = num_invitations |> NumberOfInvitations.increment
      }
  | ProfileUpdateTriggeredAtUpdated contacts ->
    contacts |> CCList.map id |> Repo.update_profile_updated_triggered pool
  | SessionParticipationSet
      ( ({ num_show_ups; num_participations; _ } as contact)
      , { show_up; participated } ) ->
    let num_show_ups =
      if show_up
      then num_show_ups |> NumberOfShowUps.increment
      else num_show_ups
    in
    let num_participations =
      if participated
      then num_participations |> NumberOfParticipations.increment
      else num_participations
    in
    { contact with num_show_ups; num_participations } |> Repo.update pool
;;
