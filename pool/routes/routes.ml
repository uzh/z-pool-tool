open Sihl.Web

let global_middlewares =
  [ Middleware.id ()
  ; Middleware.error ()
  ; Middleware.trailing_slash ()
  ; Middleware.static_file ()
  ; Middleware.csrf ()
  ; Middleware.flash ()
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
    ; post "/tenant/:id/update-detail" Handler.Root.Tenant.Update.update_detail
    ; post
        "/tenant/:id/update-database"
        Handler.Root.Tenant.Update.update_database
    ; post "/tenant/:id/create-operator" Handler.Root.Tenant.create_operator
    ; post "/root/create" Handler.Root.Root.create
    ; post "/root/:id/toggle-status" Handler.Root.Root.toggle_status
    ]
  ;;
end

let router =
  choose
    [ choose Public.routes
    ; choose ~scope:"/root" Root.routes
    ; get "/**" Handler.Public.not_found
    ]
;;
