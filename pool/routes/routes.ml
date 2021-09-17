open Sihl.Web

let global_middlewares = []

module Public = struct
  let routes = []
end

let router = choose [ choose ~middlewares:global_middlewares Public.routes ]
