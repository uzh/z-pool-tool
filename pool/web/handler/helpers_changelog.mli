val htmx_handler
  :  version_history:(module Changelog.TSig)
  -> url:string
  -> Pool_common.Id.t
  -> Rock.Request.t
  -> Rock.Response.t Lwt.t
