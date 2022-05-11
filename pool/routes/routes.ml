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
              [ CustomMiddleware.Context.context `Contact ()
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

module Contact = struct
  module SignUp = Handler.Contact.SignUp
  module UserProfile = Handler.Contact.UserProfile
  module Experiment = Handler.Contact.Experiment

  let public =
    [ get "/signup" SignUp.sign_up
    ; post "/signup" SignUp.sign_up_create
    ; get "/email-confirmation" Handler.Public.email_confirmation_note
    ; get "/termsandconditions" SignUp.terms
    ; get "/email-verified" SignUp.email_verification
    ; post "/terms-accepted/:id" SignUp.terms_accept
    ]
  ;;

  let locked_routes =
    let experiments =
      let build_scope subdir =
        Format.asprintf
          "/%s/%s"
          Pool_common.Message.Field.(Experiment |> url_key)
          subdir
      in
      let waiting_list =
        [ post "" Experiment.WaitingList.create
        ; post "/remove" Experiment.WaitingList.delete
        ]
      in
      let sessions =
        let open Pool_common.Message.Field in
        [ get (Id |> url_key) Experiment.Session.show
        ; post (Id |> url_key) Experiment.Assignment.create
        ]
      in
      [ get "" Experiment.index
      ; get Pool_common.Message.Field.(Id |> url_key) Experiment.show
      ; choose ~scope:(build_scope "waiting-list") waiting_list
      ; choose ~scope:(build_scope "sessions") sessions
      ]
    in
    [ get "/dashboard" Handler.Contact.dashboard
    ; get "/user" UserProfile.details
    ; get "/user/edit" UserProfile.edit
    ; post "/user/update" UserProfile.update
    ; post "/user/update-email" UserProfile.update_email
    ; post "/user/update-password" UserProfile.update_password
    ; choose ~scope:"/experiments" experiments
    ]
  ;;

  let routes =
    choose
      ~middlewares:
        [ CustomMiddleware.Context.context `Contact ()
        ; CustomMiddleware.Tenant.valid_tenant ()
        ]
      [ choose public
      ; choose
          ~middlewares:
            [ CustomMiddleware.Contact.confirmed_and_terms_agreed () ]
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
    let build_scope subdir =
      Format.asprintf
        "/%s/%s"
        Pool_common.Message.Field.(Experiment |> url_key)
        subdir
    in
    let experiments =
      let invitations =
        [ get "" Handler.Admin.Experiments.Invitations.index
        ; post "" Handler.Admin.Experiments.Invitations.create
        ; post "/:id/resend" Handler.Admin.Experiments.Invitations.resend
        ]
      in
      let sessions =
        let specific =
          [ get "" Handler.Admin.Session.show
          ; post "" Handler.Admin.Session.update
          ; get "/edit" Handler.Admin.Session.edit
          ; post "/cancel" Handler.Admin.Session.cancel
          ; post "/delete" Handler.Admin.Session.delete
          ]
        in
        [ get "" Handler.Admin.Session.list
        ; post "" Handler.Admin.Session.create
        ; choose ~scope:"/:session" specific
        ]
      in
      let waiting_list =
        [ get "" Handler.Admin.Experiments.WaitingList.index ]
      in
      [ get "" Handler.Admin.Experiments.index
      ; get "/new" Handler.Admin.Experiments.new_form
      ; post "" Handler.Admin.Experiments.create
      ; get "/:id" Handler.Admin.Experiments.show
      ; get "/:id/edit" Handler.Admin.Experiments.edit
      ; post "/:id" Handler.Admin.Experiments.update
      ; post "/:id/delete" Handler.Admin.Experiments.delete
      ; choose ~scope:(build_scope "invitations") invitations
      ; choose ~scope:(build_scope "waiting-list") waiting_list
      ; choose ~scope:(build_scope "sessions") sessions
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
    ; Contact.routes
    ; choose ~scope:"/admin" [ Admin.routes ]
    ; choose ~scope:"/root" [ Root.routes ]
    ; Public.global_routes
    ; get
        "/**"
        ~middlewares:[ CustomMiddleware.Context.context `Contact () ]
        Handler.Public.not_found
    ]
;;
