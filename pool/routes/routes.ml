module CustomMiddleware = Middleware
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
  let routes =
    [ get "/" Handler.Public.index
    ; get "/login" Handler.Public.Login.login_get
    ; post "/login" Handler.Public.Login.login_post
    ; get "/logout" Handler.Public.Login.logout
    ; get
        "/request-reset-password"
        Handler.Public.Login.request_reset_password_get
    ; post
        "/request-reset-password"
        Handler.Public.Login.request_reset_password_post
    ; get "/reset-password" Handler.Public.Login.reset_password_get
    ; post "/reset-password" Handler.Public.Login.reset_password_post
    ]
  ;;
end

module Participant = struct
  let middlewares =
    [ CustomMiddleware.Participant.confirmed ()
    ; CustomMiddleware.TermsAndConditions.terms_accepted ()
    ]
  ;;

  let routes =
    [ get "/signup" Handler.Participant.sign_up
    ; post "/signup" Handler.Participant.sign_up_create
    ; get "/email-confirmation" Handler.Public.email_confirmation_note
    ; get "/termsandconditions" Handler.Participant.terms
    ; post "/email-verified" Handler.Participant.email_verification
    ; post ":id/terms-accepted" Handler.Participant.terms_accept
    ]
  ;;

  let locked_routes = [ get "/dashboard" Handler.Participant.dashboard ]
end

module Admin = struct
  let middlewares =
    [ CustomMiddleware.Admin.require_admin ~login_path_f:(fun _ ->
          Sihl.Web.externalize_path "/login")
    ]
  ;;

  let routes = [ get "/dashboard" Handler.Admin.dashboard ]
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
    ; Admin.(choose ~scope:"/admin" ~middlewares routes)
    ; Participant.(choose ~scope:"/participant" routes)
    ; Participant.(choose ~scope:"/participant" ~middlewares locked_routes)
    ; choose ~scope:"/root" Root.routes
    ; get "/**" Handler.Public.not_found
    ]
;;
