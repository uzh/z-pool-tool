type url_encoded = (string * string list) list

(** Represents various types of HTTP errors that can occur during request handling. *)
type http_error =
  | AccessDenied
  (** The request was denied due to insufficient permissions. Responds with a error note and a 401 status code
  *)
  | BadRequest of
      (Rock.Request.t -> Rock.Response.t Lwt.t)
      * url_encoded option
      * Pool_message.Error.t
  (** The request was invalid, optionally including URL-encoded data and an error message.
      The passed request handler will be called and the error will be shown as a notification.
      Intended to be used with POST requests to rerender the submitted form *)
  | NotFound of Pool_message.Error.t
  (** The requested resource was not found. Responds with an error note and a 404 status code.
  *)
  | RenderError of Pool_message.Error.t
  (** Renders the occured error with a 400 status code. Intendet to be used to handle errors of GET requests
  *)

val access_denied : http_error
val not_found : Pool_message.Error.t -> http_error

val not_found_on_error
  :  ('a, Pool_message.Error.t) Lwt_result.t
  -> ('a, http_error) Lwt_result.t

val bad_request
  :  ?urlencoded:url_encoded
  -> (Rock.Request.t -> Rock.Response.t Lwt.t)
  -> Pool_message.Error.t
  -> http_error

(** [bad_request_on_error ?urlencoded fallback handler] runs [handler]. If an error occurs, [fallback] will be called.
    [urlencoded] can be passed to fill the current form values. Intended to be used with POST requests.
*)
val bad_request_on_error
  :  ?urlencoded:url_encoded
  -> (Rock.Request.t -> Rock.Response.t Lwt.t)
  -> ('a, Pool_message.Error.t) Lwt_result.t
  -> ('a, http_error) Lwt_result.t

val render_error : Pool_message.Error.t -> http_error

(** [render_error_on_error handler] runs [handler] and responds with a note that shows the error and a 400 status code if an error occurs.
    Intended to be used with GET requests. *)
val render_error_on_error
  :  ('a, Pool_message.Error.t) Lwt_result.t
  -> ('a, http_error) Lwt_result.t

val handle
  :  ?src:Logs.src
  -> ?enable_cache:bool
  -> Rock.Request.t
  -> (Pool_context.t -> (Rock.Response.t, http_error) Lwt_result.t)
  -> Rock.Response.t Lwt.t

module Api : sig
  val respond_error
    :  ?status:Opium.Status.t
    -> ?language:Pool_common.Language.t
    -> Pool_message.Error.t
    -> Rock.Response.t

  val not_found : Rock.Request.t -> Rock.Response.t Lwt.t

  val respond
    :  ?src:Logs.src
    -> Rock.Request.t
    -> (Pool_context.Api.t -> (Yojson.Safe.t, Pool_message.Error.t) Lwt_result.t)
    -> Rock.Response.t Lwt.t

  val index_handler
    :  query:(module Http_utils.Queryable.Queryable)
    -> ?src:Logs.src
    -> yojson_of_t:('a -> Yojson.Safe.t)
    -> Rock.Request.t
    -> (Pool_context.Api.t
        -> Guard.Actor.t
        -> Query.t
        -> ('a list * Query.t, Pool_message.Error.t) Lwt_result.t)
    -> Rock.Response.t Lwt.t
end

module Htmx : sig
  val html_to_plain_text_response
    :  ?status:Opium.Status.t
    -> 'a Tyxml_html.elt
    -> Rock.Response.t

  val html_list_to_plain_text_response
    :  ?status:Opium.Status.t
    -> 'a Tyxml_html.elt list
    -> Rock.Response.t

  val index_handler
    :  ?active_navigation:string
    -> query:(module Http_utils.Queryable.Queryable)
    -> create_layout:
         (Rock.Request.t
          -> ?active_navigation:string
          -> Pool_context.t
          -> ([> Html_types.div ] as 'a) Tyxml_html.elt
          -> (Html_types.html Tyxml_html.elt, Pool_message.Error.t) Lwt_result.t)
    -> Rock.Request.t
    -> (Pool_context.t
        -> Query.t
        -> ('a Tyxml_html.elt, Pool_message.Error.t) Lwt_result.t)
    -> Rock.Response.t Lwt.t
end
