open Entity

type create =
  { title : Title.t
  ; public_title : PublicTitle.t
  ; internal_description : InternalDescription.t option
  ; public_description : PublicDescription.t option
  ; language : Pool_common.Language.t option
  ; cost_center : CostCenter.t option
  ; contact_email : Pool_user.EmailAddress.t option
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; allow_uninvited_signup : AllowUninvitedSignup.t
  ; external_data_required : ExternalDataRequired.t
  ; show_external_data_id_links : ShowExternalDataIdLinks.t
  ; experiment_type : Pool_common.ExperimentType.t option
  ; email_session_reminder_lead_time : int option
  ; email_session_reminder_lead_time_unit : Pool_common.Model.TimeUnit.t option
  ; text_message_session_reminder_lead_time : int option
  ; text_message_session_reminder_lead_time_unit :
      Pool_common.Model.TimeUnit.t option
  }
[@@deriving eq, show]

type event =
  | Created of t
  | Updated of t
  | ResetInvitations of t
  | Deleted of Common.Id.t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Pool_database.to_ctx pool in
  function
  | Created t ->
    let%lwt () = Repo.insert pool t in
    Entity_guard.Target.to_authorizable ~ctx t
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated t -> Repo.update pool t
  | ResetInvitations t ->
    let%lwt experiment =
      Repo.find pool t.id ||> Pool_common.Utils.get_or_failwith
    in
    Repo.update
      pool
      { experiment with
        invitation_reset_at = Some (InvitationResetAt.create_now ())
      }
  | Deleted experiment_id -> Repo.delete pool experiment_id
[@@deriving eq, show]
;;
