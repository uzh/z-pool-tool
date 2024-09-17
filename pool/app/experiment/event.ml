open Entity

type command =
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
  ; assignment_without_session : AssignmentWithoutSession.t
  ; survey_url : SurveyUrl.t option
  ; email_session_reminder_lead_time : int option
  ; email_session_reminder_lead_time_unit : Pool_model.Base.TimeUnit.t option
  ; text_message_session_reminder_lead_time : int option
  ; text_message_session_reminder_lead_time_unit :
      Pool_model.Base.TimeUnit.t option
  }
[@@deriving eq, show]

let create_changelog pool ?user_uuid before after =
  let open Version_history in
  user_uuid
  |> CCOption.map_or ~default:Lwt.return_unit (fun user_uuid ->
    insert
      pool
      ~entity_uuid:(Entity.Id.to_common before.id)
      ~user_uuid
      ~before
      ~after
      ())
;;

type event =
  | Created of t
  | Updated of t
  | ResetInvitations of t
  | Deleted of Pool_common.Id.t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Database.to_ctx pool in
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
