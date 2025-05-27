val require_user_type_of : Pool_context.UserType.t list -> Rock.Middleware.t
val validate_admin_entity : ?any_id:CCBool.t -> Guard.ValidationSet.t -> Rock.Middleware.t

val validate_generic
  :  ?any_id:CCBool.t
  -> (Rock.Request.t -> (Guard.ValidationSet.t, Pool_message.Error.t) result)
  -> Rock.Middleware.t

val validate_generic_lwt
  :  ?any_id:CCBool.t
  -> (Rock.Request.t -> (Guard.ValidationSet.t, Pool_message.Error.t) Lwt_result.t)
  -> Rock.Middleware.t

val id_effects
  :  (string -> ('a, Pool_message.Error.t) result)
  -> Pool_message.Field.t
  -> ('a -> Guard.ValidationSet.t)
  -> Rock.Middleware.t

val api_validate_admin_entity
  :  ?any_id:CCBool.t
  -> Guard.ValidationSet.t
  -> Rock.Middleware.t

val api_id_effects
  :  (string -> ('a, Pool_message.Error.t) result)
  -> Pool_message.Field.t
  -> ('a -> Guard.ValidationSet.t)
  -> Rock.Middleware.t

val denied : Rock.Middleware.t
