type url_encoded = (string * string list) list

type http_error =
  | AccessDenied
  | BadRequest of
      (Rock.Request.t -> Rock.Response.t Lwt.t)
      * url_encoded option
      * Pool_message.Error.t
  | NotFound of Pool_message.Error.t

val access_denied : http_error

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

(** [bad_request_render_error context handler] will create a [BadRequest] if an error occurs without the need of passing a fallback handler.
    This will respond with a note that shows the occured error and a status code 400. Intended to be used with GET requests.
*)
val bad_request_render_error
  :  Pool_context.t
  -> ('a, Pool_message.Error.t) Lwt_result.t
  -> ('a, http_error) Lwt_result.t

(** [generic_not_found] renders a note that says page not found without the resource. *)
val generic_not_found : http_error

(** [empty_not_found] returns an empty Rock.Response.t with status 404 *)
val empty_not_found : Rock.Response.t

val not_found : Pool_message.Error.t -> http_error

val not_found_on_error
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

  val handle
    :  ?src:Logs.src
    -> ?error_as_notification:bool
    -> Rock.Request.t
    -> (Pool_context.t -> (Rock.Response.t, Pool_message.Error.t) Lwt_result.t)
    -> Rock.Response.t Lwt.t
end
