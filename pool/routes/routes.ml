open Sihl.Web

let global_middlewares =
  [ Middleware.id ()
  ; Middleware.error ()
  ; Middleware.trailing_slash ()
  ; Middleware.static_file ()
  ; Opium.Middleware.content_length
  ; Opium.Middleware.etag
  ; Opium.Middleware.method_override
  ]
;;

module Public = struct
  let routes = [ get "/" Handler.Public.index ]
end

let router = choose [ choose Public.routes; get "/**" Handler.Public.not_found ]
