val find_roles
  :  Database.Label.t
  -> Guard.Actor.t option
  -> (Guard.ActorRole.t * Guard.Persistence.target_model option * string option) list
       Lwt.t

val find_roles_of_ctx
  :  Pool_context.t
  -> (Guard.ActorRole.t * Guard.Persistence.target_model option * string option) list
       Lwt.t

val has_permission
  :  Database.Label.t
  -> Pool_context.user
  -> Guard.ValidationSet.t
  -> bool Lwt.t

val can_read_contact : Guard.PermissionOnTarget.t list -> Pool_context.t -> bool
val can_read_contact_name : Pool_context.t -> Guard.Uuid.Target.t list -> bool
val can_read_contact_info : Pool_context.t -> Guard.Uuid.Target.t list -> bool
val can_access_contact_profile : Pool_context.t -> Experiment.Id.t -> bool

val target_model_for_actor_role
  :  Database.Label.t
  -> Guard.ActorRole.t
  -> (Guard.ActorRole.t * Guard.Persistence.target_model option) Lwt.t

val can_send_direct_message : Pool_context.t -> bool Lwt.t

val can_rerun_session_filter
  :  Pool_context.t
  -> Experiment.Id.t
  -> Session.Id.t
  -> bool Lwt.t

val grant_role
  :  redirect_path:string
  -> user:Pool_context.user
  -> target_id:Guard.Uuid.Actor.t
  -> Database.Label.t
  -> Rock.Request.t
  -> (Rock.Response.t, Pool_message.Error.t) Lwt_result.t

val revoke_role
  :  redirect_path:string
  -> user:Pool_context.user
  -> target_id:Guard.Uuid.Actor.t
  -> Database.Label.t
  -> Rock.Request.t
  -> (Rock.Response.t, Pool_message.Error.t) Lwt_result.t

val handle_toggle_role : Guard.Uuid.Target.t -> Rock.Request.t -> Rock.Response.t Lwt.t
val search_role_entities : Guard.Target.t -> Rock.Request.t -> Rock.Response.t Lwt.t
