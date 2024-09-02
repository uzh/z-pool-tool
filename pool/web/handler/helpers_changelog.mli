val htmx_handler
  :  changelog:(module Http_utils.Queryable)
  -> active_navigation:string
  -> error_path:string
  -> url:string
  -> Pool_common.Id.t
  -> Rock.Request.t
  -> Rock.Response.t Lwt.t
