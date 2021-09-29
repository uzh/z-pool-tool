open Sihl.Web

let global_middlewares = [ Middleware.trailing_slash () ]

module Public = struct
  let routes = []
end

let router = choose [ choose ~middlewares:global_middlewares Public.routes ]
