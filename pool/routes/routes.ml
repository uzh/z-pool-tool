module CustomMiddleware = Middleware
open Sihl.Web

let global_middlewares =
  [ Middleware.id ()
  ; CustomMiddleware.Error.error ()
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
  let global_routes =
    choose
      [ get "/" Handler.Public.root_redirect
      ; get "/custom/assets/:id/:filename" Handler.Public.asset
      ; get "/error" Handler.Public.error
      ]
  ;;

  let routes =
    Handler.Public.(
      choose
        [ choose
            ~middlewares:
              [ CustomMiddleware.Context.context `Participant ()
              ; CustomMiddleware.Tenant.valid_tenant ()
              ]
            [ get "/index" index
            ; get "/custom/assets/index.css" index_css
            ; get "/login" Login.login_get
            ; post "/login" Login.login_post
            ; get "/logout" Login.logout
            ; get "/request-reset-password" Login.request_reset_password_get
            ; post "/request-reset-password" Login.request_reset_password_post
            ; get "/reset-password" Login.reset_password_get
            ; post "/reset-password" Login.reset_password_post
            ]
        ])
  ;;
end

module Participant = struct
  let public =
    [ get "/signup" Handler.Participant.sign_up
    ; post "/signup" Handler.Participant.sign_up_create
    ; get "/email-confirmation" Handler.Public.email_confirmation_note
    ; get "/termsandconditions" Handler.Participant.terms
    ; get "/email-verified" Handler.Participant.email_verification
    ; post "/terms-accepted/:id" Handler.Participant.terms_accept
    ]
  ;;

  let locked_routes =
    [ get "/dashboard" Handler.Participant.dashboard
    ; get "/user" Handler.Participant.details
    ; get "/user/edit" Handler.Participant.edit
    ; post "/user/update" Handler.Participant.update
    ; post "/user/update-email" Handler.Participant.update_email
    ; post "/user/update-password" Handler.Participant.update_password
    ]
  ;;

  let routes =
    choose
      ~middlewares:
        [ CustomMiddleware.Context.context `Participant ()
        ; CustomMiddleware.Tenant.valid_tenant ()
        ]
      [ choose public
      ; choose
          ~middlewares:
            [ CustomMiddleware.Participant.confirmed_and_terms_agreed () ]
          locked_routes
      ]
  ;;
end

module Admin = struct
  let middlewares =
    [ CustomMiddleware.Context.context `Admin ()
    ; CustomMiddleware.Tenant.valid_tenant ()
    ; CustomMiddleware.Admin.require_admin ~login_path_f:(fun () -> "/login")
    ]
  ;;

  let routes =
    choose
      ~middlewares
      [ get "/dashboard" Handler.Admin.dashboard
      ; get "/settings" Handler.Admin.Settings.show
      ; post "/settings/:action" Handler.Admin.Settings.update_settings
      ; get "/i18n" Handler.Admin.I18n.index
      ; post "/i18n/:id" Handler.Admin.I18n.update
      ; get "/experiments" Handler.Admin.Experiments.index
      ; get "/experiments/new" Handler.Admin.Experiments.new_form
      ; post "/experiments" Handler.Admin.Experiments.create
      ; get "/experiments/:id" Handler.Admin.Experiments.show
      ; get "/experiments/:id/edit" Handler.Admin.Experiments.edit
      ; post "/experiments/:id" Handler.Admin.Experiments.update
      ; post "/experiments/:id/delete" Handler.Admin.Experiments.delete
      ]
  ;;
end

module Root = struct
  let middlewares =
    [ CustomMiddleware.Root.from_root_only ()
    ; CustomMiddleware.Context.context `Root ()
    ]
  ;;

  let public_routes =
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
    [ CustomMiddleware.Root.require_root ~login_path_f:(fun () -> "/root/login")
    ]
  ;;

  let locked_routes =
    let open Handler.Root in
    [ get "/tenants" Tenant.tenants
    ; post "/tenants/create" Tenant.create
    ; get "/tenants/:id" Tenant.tenant_detail
    ; post "/tenants/:id/create-operator" Tenant.create_operator
    ; post "/tenants/:id/update-detail" Tenant.Update.update_detail
    ; post "/tenants/:id/update-database" Tenant.Update.update_database
    ; post
        "/tenants/:tenant_id/assets/:asset_id/delete"
        Tenant.Update.delete_asset
    ; post "/root/create" Root.create
    ; post "/root/:id/toggle-status" Root.toggle_status
    ]
  ;;

  let routes =
    choose
      ~middlewares
      [ choose public_routes
      ; choose ~middlewares:locked_middlewares locked_routes
      ]
  ;;
end

let router =
  choose
    [ Public.routes
    ; Participant.routes
    ; choose ~scope:"/admin" [ Admin.routes ]
    ; choose ~scope:"/root" [ Root.routes ]
    ; Public.global_routes
    ; get
        "/**"
        ~middlewares:[ CustomMiddleware.Context.context `Participant () ]
        Handler.Public.not_found
    ]
;;
