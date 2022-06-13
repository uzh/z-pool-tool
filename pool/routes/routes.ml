module CustomMiddleware = Middleware
open Sihl.Web

let add_key ?(prefix = "") ?(suffix = "") field =
  let open Pool_common.Message.Field in
  [ prefix; field |> url_key; suffix ]
  |> CCList.filter (fun m -> m |> CCString.is_empty |> not)
  |> CCString.concat "/"
;;

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
  module WaitingList = Handler.Contact.WaitingList
  module Session = Handler.Contact.Session
  module Assignment = Handler.Contact.Assignment

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
        [ post "" WaitingList.create; post "/remove" WaitingList.delete ]
      in
      let sessions =
        let open Pool_common.Message.Field in
        [ get (Session |> url_key) Session.show
        ; post (Session |> url_key) Assignment.create
        ]
      in
      [ get "" Experiment.index
      ; get Pool_common.Message.Field.(Experiment |> url_key) Experiment.show
      ; choose ~scope:(build_scope "waiting-list") waiting_list
      ; choose ~scope:(build_scope "sessions") sessions
      ]
    in
    [ get "/dashboard" Handler.Contact.dashboard
    ; get "/user" UserProfile.details
    ; get "/user/personal-details" UserProfile.personal_details
    ; get "/user/login-information" UserProfile.login_information
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
    let open Pool_common.Message.Field in
    let location =
      let open Handler.Admin.Location in
      let files =
        [ get "/create" new_file
        ; post "" add_file
        ; choose ~scope:(add_key File) [ get "" asset ]
        ]
      in
      let specific =
        [ get "" show
        ; get "/edit" edit
        ; post "" update
        ; choose ~scope:"/files" files
        ; choose
            ~scope:(add_key ~prefix:"mapping" FileMapping)
            [ post "/delete" delete ]
        ]
      in
      [ get "" index
      ; get "/create" new_form
      ; post "" create
      ; choose ~scope:(add_key Location) specific
      ]
    in
    let experiments =
      let open Handler.Admin.Experiments in
      let build_scope subdir =
        Format.asprintf "/%s/%s" (Experiment |> url_key) subdir
      in
      let invitations =
        let open Invitations in
        [ get "" index
        ; post "" create
        ; post (add_key ~suffix:"resend" Invitation) resend
        ]
      in
      let sessions =
        let open Handler.Admin.Session in
        let specific =
          [ get "" show
          ; post "" update
          ; get "/edit" edit
          ; post "/cancel" cancel
          ; post "/delete" delete
          ]
        in
        [ get "" list; post "" create; choose ~scope:"/:session" specific ]
      in
      let waiting_list =
        let open WaitingList in
        let specific =
          [ post "" update; get "" detail; post "/assign" assign_contact ]
        in
        [ get "" index; choose ~scope:(WaitingList |> url_key) specific ]
      in
      let assignments =
        let open Assignment in
        let specific = [ post "/cancel" cancel ] in
        [ get "" index; choose ~scope:(Assignment |> url_key) specific ]
      in
      let mailings =
        let open Mailings in
        let specific = [ get "" show; post "" update ] in
        [ get "" index
        ; post "" create
        ; get "/create" new_form
        ; post "/search-info" search_info
        ; choose ~scope:(Mailing |> url_key) specific
        ]
      in
      [ get "" index
      ; get "/create" new_form
      ; post "" create
      ; get (Experiment |> url_key) show
      ; get (add_key ~suffix:"edit" Experiment) edit
      ; post (Experiment |> url_key) update
      ; post (add_key ~suffix:"delete" Experiment) delete
      ; choose ~scope:(build_scope "invitations") invitations
      ; choose ~scope:(build_scope "waiting-list") waiting_list
      ; choose ~scope:(build_scope "sessions") sessions
      ; choose ~scope:(build_scope "assignments") assignments
      ; choose ~scope:(build_scope "mailings") mailings
      ]
    in
    choose
      ~middlewares
      [ get "/dashboard" Handler.Admin.dashboard
      ; get "/settings" Handler.Admin.Settings.show
      ; post "/settings/:action" Handler.Admin.Settings.update_settings
      ; get "/i18n" Handler.Admin.I18n.index
      ; post
          (Format.asprintf "/i18n/%s" (I18n |> url_key))
          Handler.Admin.I18n.update
      ; choose ~scope:"/experiments" experiments
      ; choose ~scope:"/locations" location
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
    let open Pool_common.Message.Field in
    let open Handler.Root in
    let build_route appendix =
      Format.asprintf "%s/%s" (Tenant |> url_key) appendix
    in
    let tenants =
      [ get "" Tenant.tenants
      ; post "/create" Tenant.create
      ; get (Tenant |> url_key) Tenant.tenant_detail
      ; post (build_route "create-operator") Tenant.create_operator
      ; post (build_route "update-detail") Tenant.Update.update_detail
      ; post (build_route "update-database") Tenant.Update.update_database
      ; post
          (build_route
             (Format.asprintf "assets/%s/delete" (AssetId |> url_key)))
          Tenant.Update.delete_asset
      ]
    in
    let root =
      [ post "/create" Root.create
      ; post
          (Format.asprintf "/%s/toggle-status" (Root |> url_key))
          Root.toggle_status
      ]
    in
    [ choose [ choose ~scope:"tenants" tenants; choose ~scope:"root" root ] ]
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
