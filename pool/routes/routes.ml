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
    Handler.Public.
      [ get "/" index
      ; get "/custom/assets/index.css" index_css
      ; get "/login" Login.login_get
      ; post "/login" Login.login_post
      ; get "/logout" Login.logout
      ; get "/request-reset-password" Login.request_reset_password_get
      ; post "/request-reset-password" Login.request_reset_password_post
      ; get "/reset-password" Login.reset_password_get
      ; post "/reset-password" Login.reset_password_post
      ; get "/custom/assets/:id/:filename" asset
      ; get "/custom/assets/favicon" favicon
      ; get "/error" error
      ]
  ;;
end

module Participant = struct
  let routes =
    [ get "/signup" Handler.Participant.sign_up
    ; post "/signup" Handler.Participant.sign_up_create
    ; get "/email-confirmation" Handler.Public.email_confirmation_note
    ; get "/termsandconditions" Handler.Participant.terms
    ; get "/email-verified" Handler.Participant.email_verification
    ; post "/terms-accepted/:id" Handler.Participant.terms_accept
    ; get "/user" Handler.Participant.details
    ; get "/user/edit" Handler.Participant.edit
    ; post "/user/update" Handler.Participant.update
    ; post "/user/update-email" Handler.Participant.update_email
    ; post "/user/update-password" Handler.Participant.update_password
    ]
  ;;

  let middlewares =
    [ CustomMiddleware.Participant.confirmed_and_terms_agreed () ]
  ;;

  let locked_routes = [ get "/dashboard" Handler.Participant.dashboard ]
end

module Admin = struct
  let middlewares =
    [ CustomMiddleware.Admin.require_admin ~login_path_f:(fun () -> "/login") ]
  ;;

  let routes =
    Handler.Admin.
      [ get "/dashboard" dashboard
      ; get "/settings" Settings.show
      ; post "/settings/:action" Settings.update_settings
      ]
  ;;
end

module Root = struct
  let middlewares = CustomMiddleware.Root.[ from_root_only () ]

  let routes =
    let open Handler.Root in
    [ get "" (forward_to_entrypoint "/root/tenants")
    ; get "/login" Login.login_get
    ; post "/login" Login.login_post
    ; get "/logout" Login.logout
    ; get "/request-reset-password" Login.request_reset_password_get
    ; post "/request-reset-password" Login.request_reset_password_post
    ; get "/reset-password" Login.reset_password_get
    ; post "/reset-password" Login.reset_password_post
    ]
  ;;

  let locked_middlewares =
    middlewares
    @ CustomMiddleware.Root.
        [ require_root ~login_path_f:(fun () -> "/root/login") ]
  ;;

  let locked_routes =
    let open Handler.Root in
    [ get "/tenants" Tenant.tenants
    ; post "/tenants/create" Tenant.create
    ; get "/tenants/:id" Tenant.tenant_detail
    ; post "/tenants/:id/create-operator" Tenant.create_operator
    ; post "/tenants/:id/promote-operator" Tenant.promote_to_operator
    ; post "/tenants/:id/update-detail" Tenant.Update.update_detail
    ; post "/tenants/:id/update-database" Tenant.Update.update_database
    ; post
        "/tenants/:tenant_id/assets/:asset_id/delete"
        Tenant.Update.delete_asset
    ; post "/root/create" Root.create
    ; post "/root/:id/toggle-status" Root.toggle_status
    ]
  ;;
end

let router =
  choose
    [ choose Public.routes
    ; Participant.(choose routes)
    ; Participant.(choose ~middlewares locked_routes)
    ; Admin.(choose ~scope:"/admin" ~middlewares routes)
    ; Root.(choose ~scope:"/root" ~middlewares routes)
    ; Root.(choose ~scope:"/root" ~middlewares:locked_middlewares locked_routes)
    ; get "/**" Handler.Public.not_found
    ]
;;
