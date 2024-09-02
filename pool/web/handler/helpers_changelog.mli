val htmx_handler
  :  changelog:(module Changelog.TSig)
  -> url:string
  -> Pool_common.Id.t
  -> Rock.Request.t
  -> Rock.Response.t Lwt.t
