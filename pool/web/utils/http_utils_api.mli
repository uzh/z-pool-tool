val api_request_header : string
val is_api_request : Rock.Request.t -> bool

val find_id
  :  (string -> ('a, Pool_message.Error.t) Result.t)
  -> Pool_message.Field.t
  -> Rock.Request.t
  -> ('a, Pool_message.Error.t) Result.t
