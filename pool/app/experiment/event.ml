open Entity

type create =
  { title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; allow_uninvited_signup : AllowUninvitedSignup.t
  ; experiment_type : Pool_common.ExperimentType.t option
  ; invitation_subject : InvitationTemplate.Subject.t option
  ; invitation_text : InvitationTemplate.Text.t option
  ; session_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  ; session_reminder_subject : Pool_common.Reminder.Subject.t option
  ; session_reminder_text : Pool_common.Reminder.Text.t option
  }
[@@deriving eq, show]

type event =
  | Created of t
  | Updated of t
  | Destroyed of Common.Id.t
  | AssistantAssigned of t * Admin.t
  | AssistantDivested of t * Admin.t
  | ExperimenterAssigned of t * Admin.t
  | ExperimenterDivested of t * Admin.t
[@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Pool_tenant.to_ctx pool in
  let find_id = CCFun.(Admin.id %> Guard.Uuid.actor_of Admin.Id.value) in
  let to_target { id; _ } = Guard.Uuid.target_of Id.value id in
  let grant_role role admin =
    Guard.Persistence.Actor.grant_roles
      ~ctx
      (admin |> find_id)
      (Guard.ActorRoleSet.singleton role)
    ||> CCResult.get_exn
  in
  let revoke_role role admin =
    Guard.Persistence.Actor.revoke_roles
      ~ctx
      (admin |> find_id)
      (Guard.ActorRoleSet.singleton role)
    ||> CCResult.get_exn
  in
  function
  | Created ({ id; _ } as t) ->
    let%lwt () = Repo.insert pool t in
    let%lwt (_ : Guard.Authorizer.auth_rule list) =
      Admin.Guard.RuleSet.experimenter id @ Admin.Guard.RuleSet.assistant id
      |> Guard.Persistence.Actor.save_rules ~ctx
      >|- (fun err ->
            Pool_common.Message.nothandled
            @@ Format.asprintf
                 "Failed to save: %s"
                 ([%show: Guard.Authorizer.auth_rule list] err))
      ||> Pool_common.Utils.get_or_failwith
    in
    Entity_guard.Target.to_authorizable ~ctx t
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : [> `Experiment ] Guard.AuthorizableTarget.t) -> ()
  | Updated t -> Repo.update pool t
  | Destroyed experiment_id -> Repo.destroy pool experiment_id
  | AssistantAssigned (experiment, admin) ->
    grant_role (`Assistant (experiment |> to_target)) admin
  | AssistantDivested (experiment, admin) ->
    revoke_role (`Assistant (experiment |> to_target)) admin
  | ExperimenterAssigned (experiment, admin) ->
    grant_role (`Experimenter (experiment |> to_target)) admin
  | ExperimenterDivested (experiment, admin) ->
    revoke_role (`Experimenter (experiment |> to_target)) admin
  [@@deriving eq, show]
;;
