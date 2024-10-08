val find_id
  :  (string -> ('a, Pool_message.Error.t) Result.t)
  -> Pool_message.Field.t
  -> Rock.Request.t
  -> ('a, Pool_message.Error.t) Result.t

val respond_error
  :  ?status:Opium.Status.t
  -> ?language:Pool_common.Language.t
  -> Pool_message.Error.t
  -> Rock.Response.t

val respond
  :  ?src:Logs.src
  -> Rock.Request.t
  -> (Pool_context.Api.t -> (Yojson.Safe.t, Pool_message.Error.t) Lwt_result.t)
  -> Rock.Response.t Lwt.t
