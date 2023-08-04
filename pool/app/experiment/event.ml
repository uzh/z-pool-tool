open Entity

type create =
  { title : Title.t
  ; public_title : PublicTitle.t
  ; description : Description.t option
  ; cost_center : CostCenter.t option
  ; direct_registration_disabled : DirectRegistrationDisabled.t
  ; registration_disabled : RegistrationDisabled.t
  ; allow_uninvited_signup : AllowUninvitedSignup.t
  ; external_data_required : ExternalDataRequired.t
  ; experiment_type : Pool_common.ExperimentType.t option
  ; session_reminder_lead_time : Pool_common.Reminder.LeadTime.t option
  }
[@@deriving eq, show]

type event =
  | Created of t
  | Updated of t
  | Deleted of Common.Id.t
[@@deriving eq, show, variants]

let handle_event pool : event -> unit Lwt.t =
  let open CCFun in
  let open Utils.Lwt_result.Infix in
  let ctx = Pool_database.to_ctx pool in
  function
  | Created ({ id; _ } as t) ->
    let%lwt () = Repo.insert pool t in
    let%lwt (_ : Guard.Rule.t list) =
      let open Guard.Persistence in
      Admin.Guard.RuleSet.experimenter id @ Admin.Guard.RuleSet.assistant id
      |> Rule.save_all ~ctx
      >|- [%show: Guard.Rule.t list]
          %> Format.asprintf "Failed to save: %s"
          %> Pool_common.Message.nothandled
      ||> tap (fun _ -> Cache.clear ())
      ||> Pool_common.Utils.get_or_failwith
    in
    Entity_guard.Target.to_authorizable ~ctx t
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : [> `Experiment ] Guard.Target.t) -> ()
  | Updated t -> Repo.update pool t
  | Deleted experiment_id -> Repo.delete pool experiment_id
[@@deriving eq, show]
;;
