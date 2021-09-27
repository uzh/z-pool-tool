open Sihl.Web

let global_middlewares =
  [ Middleware.id ()
  ; Middleware.flash ()
  ; Middleware.csrf ()
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

module Root = struct
  let routes =
    [ get "/tenants" Handler.Root.Tenant.tenants
    ; post "/tenant/create" Handler.Root.Tenant.create
    ; get "/tenant/:id" Handler.Root.Tenant.tenant_detail
    ; post "/tenant/:id/update-detail" Handler.Root.Tenant.update_detail
    ; post "/tenant/:id/update-database" Handler.Root.Tenant.update_database
    ]
  ;;
end

let router =
  choose
    [ choose ~middlewares:global_middlewares Public.routes
    ; choose ~scope:"/root" ~middlewares:global_middlewares Root.routes
    ]
;;
