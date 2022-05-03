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
              [ CustomMiddleware.Context.context `Subject ()
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

module Subject = struct
  let public =
    [ get "/signup" Handler.Subject.sign_up
    ; post "/signup" Handler.Subject.sign_up_create
    ; get "/email-confirmation" Handler.Public.email_confirmation_note
    ; get "/termsandconditions" Handler.Subject.terms
    ; get "/email-verified" Handler.Subject.email_verification
    ; post "/terms-accepted/:id" Handler.Subject.terms_accept
    ]
  ;;

  let locked_routes =
    [ get "/dashboard" Handler.Subject.dashboard
    ; get "/user" Handler.Subject.details
    ; get "/user/edit" Handler.Subject.edit
    ; post "/user/update" Handler.Subject.update
    ; post "/user/update-email" Handler.Subject.update_email
    ; post "/user/update-password" Handler.Subject.update_password
    ]
  ;;

  let routes =
    choose
      ~middlewares:
        [ CustomMiddleware.Context.context `Subject ()
        ; CustomMiddleware.Tenant.valid_tenant ()
        ]
      [ choose public
      ; choose
          ~middlewares:
            [ CustomMiddleware.Subject.confirmed_and_terms_agreed () ]
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
    let experiments =
      let invitations =
        [ get "" Handler.Admin.Experiments.Invitations.index
        ; post "" Handler.Admin.Experiments.Invitations.create
        ; post "/:id/resend" Handler.Admin.Experiments.Invitations.resend
        ]
      in
      [ get "" Handler.Admin.Experiments.index
      ; get "/new" Handler.Admin.Experiments.new_form
      ; post "" Handler.Admin.Experiments.create
      ; get "/:id" Handler.Admin.Experiments.show
      ; get "/:id/edit" Handler.Admin.Experiments.edit
      ; post "/:id" Handler.Admin.Experiments.update
      ; post "/:id/delete" Handler.Admin.Experiments.delete
      ; choose
          ~scope:
            (Format.asprintf
               "/%s/invitations"
               Pool_common.Message.Field.(Experiment |> url_key))
          invitations
      ]
    in
    choose
      ~middlewares
      [ get "/dashboard" Handler.Admin.dashboard
      ; get "/settings" Handler.Admin.Settings.show
      ; post "/settings/:action" Handler.Admin.Settings.update_settings
      ; get "/i18n" Handler.Admin.I18n.index
      ; post "/i18n/:id" Handler.Admin.I18n.update
      ; choose ~scope:"/experiments" experiments
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
    ; Subject.routes
    ; choose ~scope:"/admin" [ Admin.routes ]
    ; choose ~scope:"/root" [ Root.routes ]
    ; Public.global_routes
    ; get
        "/**"
        ~middlewares:[ CustomMiddleware.Context.context `Subject () ]
        Handler.Public.not_found
    ]
;;
