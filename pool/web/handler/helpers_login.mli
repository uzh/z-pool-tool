type login_step =
  | MfaRequired of Pool_user.t * Authentication.t * Pool_event.t list
  | DirectLogin of Pool_user.t

(** [initiate_login req context urlencoded] validates the submitted credentials
    and decides, based on the SMTP configuration and the user type/permissions,
    whether the login has to continue with a 2FA step ([MfaRequired]) or can log
    the user in directly ([DirectLogin]). *)
val initiate_login
  :  ?id:Authentication.Id.t
  -> ?token:Authentication.Token.t
  -> ?tags:Logs.Tag.set
  -> Rock.Request.t
  -> Pool_context.t
  -> (string * string list) list
  -> (login_step, Pool_message.Error.t) Lwt_result.t

(** [create_2fa_login req context urlencoded] validates the submitted credentials
    and unconditionally creates a 2FA authentication with the corresponding email
    job events. Used by entry points that always require a second factor (root). *)
val create_2fa_login
  :  ?id:Authentication.Id.t
  -> ?token:Authentication.Token.t
  -> ?tags:Logs.Tag.set
  -> Rock.Request.t
  -> Pool_context.t
  -> (string * string list) list
  -> ( Pool_user.t * Authentication.t * Pool_event.t list
       , Pool_message.Error.t )
       Lwt_result.t

(** [decode_2fa_confirmation database_label req ~tags] decodes the submitted
    authentication id and token and looks up the corresponding (still valid)
    authentication and user. *)
val decode_2fa_confirmation
  :  Database.Label.t
  -> Rock.Request.t
  -> tags:Logs.Tag.set
  -> ( Pool_user.t * Authentication.t * Authentication.Token.t
       , Pool_message.Error.t )
       Lwt_result.t

(** [confirm_2fa_login ~tags user authentication token req] confirms the 2FA [token]
    for [authentication] and returns the resulting login events. *)
val confirm_2fa_login
  :  tags:Logs.Tag.set
  -> Pool_user.t
  -> Authentication.t
  -> Authentication.Token.t
  -> Rock.Request.t
  -> (Pool_user.t * Pool_event.t list, Pool_message.Error.t) Lwt_result.t

(** [login_verify_get ~render_confirmation ~login_path req] renders the 2FA token
    confirmation page for the authentication stored in the session.

    [render_confirmation context auth user] produces the response
    A missing/expired authentication redirects to [login_path]. *)
val login_verify_get
  :  render_confirmation:
       (Pool_context.t
        -> Authentication.t
        -> Pool_user.t
        -> (Rock.Response.t, Http_response.http_error) Lwt_result.t)
  -> login_path:string
  -> Rock.Request.t
  -> Rock.Response.t Lwt.t

(** [login_verify_post ~verify_path ~handle_verified ~handle_invalid_session req]
    confirms the submitted 2FA token.

    On success [handle_verified context user] is called.

    An invalid token redirects back to [verify_path].

    A missing/expired session is delegated to [handle_invalid_session]. *)
val login_verify_post
  :  verify_path:string
  -> handle_verified:
       (Pool_context.t
        -> Pool_user.t
        -> (Rock.Response.t, Http_response.http_error) Lwt_result.t)
  -> handle_invalid_session:
       (unit -> (Rock.Response.t, Http_response.http_error) Lwt_result.t)
  -> Rock.Request.t
  -> Rock.Response.t Lwt.t

(** [resend_token_post ~verify_path req] re-issues the 2FA token for the pending and
    redirects to [verify_path]. *)
val resend_token_post : verify_path:string -> Rock.Request.t -> Rock.Response.t Lwt.t
