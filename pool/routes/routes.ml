open Sihl.Web

let global_middlewares =
  [ Middleware.id ()
  ; Middleware.error ()
  ; Middleware.trailing_slash ()
  ; Middleware.static_file ()
  ; Middleware.migration Service.Migration.pending_migrations
  ; Opium.Middleware.content_length
  ; Opium.Middleware.etag
  ; Opium.Middleware.method_override
  ]
;;

module Public = struct
  let routes = [ get "/" Handler.Public.index ]
end

let router =
  choose
    [ choose ~middlewares:global_middlewares Public.routes
    ; get "/**" Handler.Public.not_found
    ]
;;
