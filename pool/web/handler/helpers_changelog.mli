val htmx_handler
  :  ?to_human:(Pool_context.t -> Changelog.t -> Changelog.t Lwt.t)
  -> url:string
  -> Pool_common.Id.t
  -> Rock.Request.t
  -> Rock.Response.t Lwt.t
