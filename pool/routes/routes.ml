module CustomMiddleware = Middleware
open Sihl.Web

let global_middlewares =
  [ Middleware.id ()
  ; Middleware.error ()
  ; CustomMiddleware.trailing_slash ()
  ; Middleware.static_file ()
  ; Middleware.migration Service.Migration.pending_migrations
  ; Opium.Middleware.content_length
  ; Opium.Middleware.etag
  ; Opium.Middleware.method_override
  ]
;;

module Public = struct
  let routes = []
end

let router = choose [ choose ~middlewares:global_middlewares Public.routes ]
