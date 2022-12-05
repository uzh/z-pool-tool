module CustomMiddleware = Middleware
open Sihl.Web

let add_key ?(prefix = "") ?(suffix = "") field =
  let open Pool_common.Message.Field in
  [ prefix; field |> url_key; suffix ]
  |> CCList.filter (fun m -> m |> CCString.is_empty |> not)
  |> CCString.concat "/"
;;

let global_middlewares =
  [ Middleware.id ~id:(fun () -> String.sub (Sihl.Random.base64 12) 0 10) ()
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
              ; CustomMiddleware.Logger.logger
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
    let locked =
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
    in
    [ choose
        ~middlewares:
          [ CustomMiddleware.Contact.completion_in_progress ()
          ; CustomMiddleware.Logger.logger
          ]
        locked
    ; get "/user/completion" UserProfile.completion
    ; post "/user/completion" UserProfile.completion_post
    ]
  ;;

  let routes =
    choose
      ~middlewares:
        [ CustomMiddleware.Context.context `Contact ()
        ; CustomMiddleware.Tenant.valid_tenant ()
        ; CustomMiddleware.Logger.logger
        ]
      [ choose public
      ; choose
          ~middlewares:
            [ CustomMiddleware.Contact.confirmed_and_terms_agreed ()
            ; CustomMiddleware.Logger.logger
            ]
          locked_routes
      ]
  ;;
end

module Admin = struct
  let middlewares =
    [ CustomMiddleware.Context.context `Admin ()
    ; CustomMiddleware.Tenant.valid_tenant ()
    ; CustomMiddleware.Admin.require_admin ()
    ; CustomMiddleware.Logger.logger
    ]
  ;;

  let routes =
    let open Pool_common.Message.Field in
    let open Handler.Admin in
    let location =
      let files =
        Location.
          [ get "/create" new_file
          ; post "" add_file
          ; choose ~scope:(add_key File) [ get "" asset ]
          ]
      in
      let specific =
        Location.
          [ get "" show
          ; get "/edit" edit
          ; post "" update
          ; choose ~scope:"/files" files
          ; choose
              ~scope:(add_key ~prefix:"mapping" FileMapping)
              Location.[ post "/delete" delete ]
          ]
      in
      Location.
        [ get "" index
        ; get "/create" new_form
        ; post "" create
        ; choose ~scope:(add_key Location) specific
        ]
    in
    let experiments =
      let build_scope subdir =
        Format.asprintf "/%s/%s" (Experiment |> url_key) subdir
      in
      let invitations =
        Experiments.Invitations.
          [ get "" index
          ; post "" create
          ; get "sent" sent_invitations
          ; post (add_key ~suffix:"resend" Invitation) resend
          ]
      in
      let sessions =
        let specific =
          Session.
            [ get "" show
            ; post "" update
            ; get "/edit" edit
            ; get "/follow-up" follow_up
            ; post "/follow-up" create_follow_up
            ; post "/cancel" cancel
            ; post "/delete" delete
            ; get "/reschedule" reschedule_form
            ; post "/reschedule" reschedule
            ; get "/close" close
            ; post "/close" close_post
            ]
        in
        Session.
          [ get "" list
          ; get "/create" new_form
          ; post "" create
          ; choose ~scope:"/:session" specific
          ]
      in
      let waiting_list =
        let specific =
          Experiments.WaitingList.
            [ post "" update; get "" detail; post "/assign" assign_contact ]
        in
        Experiments.WaitingList.
          [ get "" index; choose ~scope:(WaitingList |> url_key) specific ]
      in
      let assignments =
        let specific = Experiments.Assignment.[ post "/cancel" cancel ] in
        Experiments.Assignment.
          [ get "" index; choose ~scope:(Assignment |> url_key) specific ]
      in
      let mailings =
        let specific =
          Experiments.Mailings.
            [ get "" show
            ; post "" update
            ; get "/edit" edit
            ; post "/delete" delete
            ; post "/stop" stop
            ]
        in
        Experiments.Mailings.
          [ get "" index
          ; post "" create
          ; get "/create" new_form
          ; post "/search-info" search_info
          ; post "/add-condition" add_condition
          ; choose ~scope:(Mailing |> url_key) specific
          ]
      in
      let filter =
        Handler.Admin.Filter.[ post "/create" create_for_experiment ]
      in
      Experiments.
        [ get "" index
        ; get "/create" new_form
        ; post "" create
        ; get (Experiment |> url_key) show
        ; get (add_key ~suffix:"edit" Experiment) edit
        ; post (Experiment |> url_key) update
        ; post (add_key ~suffix:"delete" Experiment) delete
        ; get
            (add_key ~suffix:"contact-count" Experiment)
            Handler.Admin.Filter.count_contacts
        ; choose ~scope:(build_scope "invitations") invitations
        ; choose ~scope:(build_scope "waiting-list") waiting_list
        ; choose ~scope:(build_scope "sessions") sessions
        ; choose ~scope:(build_scope "assignments") assignments
        ; choose ~scope:(build_scope "mailings") mailings
        ; choose ~scope:(build_scope "filter") filter
        ]
    in
    let admins =
      let specific = Handler.Admin.Admin.[ get "" detail; get "/edit" edit ] in
      Handler.Admin.Admin.
        [ get "" index; choose ~scope:(Admin |> url_key) specific ]
    in
    let contacts =
      let specific =
        Handler.Admin.Contacts.
          [ get "" detail; post "" update; get "/edit" edit ]
      in
      Handler.Admin.Contacts.
        [ get "" index; choose ~scope:(Contact |> url_key) specific ]
    in
    let filter =
      let open Handler.Admin.Filter in
      let specific = [ get "edit" edit; post "" update_template ] in
      [ get "" index
      ; post "" create_template
      ; get "/new" new_form
      ; post "/toggle-key" toggle_key
      ; post "/toggle-predicate-type" toggle_predicate_type
      ; post "/add-predicate" add_predicate
      ; choose ~scope:(Filter |> url_key) specific
      ]
    in
    let custom_fields =
      let open CustomField in
      let specific =
        let options =
          let specific =
            CustomFieldOption.
              [ get "/edit" edit
              ; post "" update
              ; post "/delete" delete
              ; post "/publish" publish
              ]
          in
          CustomFieldOption.
            [ get "/new" new_form
            ; post "" create
            ; choose ~scope:(CustomFieldOption |> url_key) specific
            ]
        in
        CustomField.
          [ get "/edit" edit
          ; post "" update
          ; post "/publish" publish
          ; post "/delete" delete
          ; post "/sort-options" sort_options
          ; choose ~scope:"options" options
          ]
      in
      let fields =
        [ post "" create
        ; get "/new" new_form
        ; choose ~scope:(CustomField |> url_key) specific
        ]
      in
      let groups =
        let specific =
          CustomFieldGroup.
            [ get "/edit" edit
            ; post "" update
            ; post "/delete" delete
            ; post "sort-fields" sort_fields
            ]
        in
        CustomFieldGroup.
          [ get "/new" new_form
          ; post "" create
          ; post "/sort" sort
          ; choose ~scope:(CustomFieldGroup |> url_key) specific
          ]
      in
      let models =
        [ get "" index
        ; post "sort-fields" sort_ungrouped_fields
        ; choose ~scope:"field" fields
        ; choose ~scope:"group" groups
        ]
      in
      [ get "" redirect; choose ~scope:(Model |> url_key) models ]
    in
    choose
      ~middlewares
      [ get "/dashboard" dashboard
      ; get "/settings" Settings.show
      ; post "/settings/:action" Settings.update_settings
      ; get "/i18n" I18n.index
      ; post (Format.asprintf "/i18n/%s" (I18n |> url_key)) I18n.update
      ; choose ~scope:"/experiments" experiments
      ; choose ~scope:"/filter" filter
      ; choose ~scope:"/locations" location
      ; choose ~scope:"/contacts" contacts
      ; choose ~scope:"/admins" admins
      ; choose ~scope:"/custom-fields" custom_fields
      ]
  ;;
end

module Root = struct
  let middlewares =
    [ CustomMiddleware.Root.from_root_only ()
    ; CustomMiddleware.Context.context `Admin ()
    ; CustomMiddleware.Logger.logger
    ]
  ;;

  let public_routes =
    let open Handler.Root in
    [ get "" (forward_to_entrypoint "/root/login")
    ; get "/login" Login.login_get
    ; post "/login" Login.login_post
    ; get "/logout" Login.logout
    ; get "/request-reset-password" Login.request_reset_password_get
    ; post "/request-reset-password" Login.request_reset_password_post
    ; get "/reset-password" Login.reset_password_get
    ; post "/reset-password" Login.reset_password_post
    ]
  ;;

  let locked_middlewares = [ CustomMiddleware.Root.require_root () ]

  let locked_routes =
    let open Pool_common.Message.Field in
    let open Handler.Root in
    let build_route appendix =
      Format.asprintf "%s/%s" (Tenant |> url_key) appendix
    in
    let tenants =
      let specific =
        Tenant.
          [ get "" tenant_detail
          ; get "operator" manage_operators
          ; post "create-operator" create_operator
          ; post "update-detail" Update.update_detail
          ; post "update-database" Update.update_database
          ]
      in
      [ get "" Tenant.tenants
      ; post "/create" Tenant.create
      ; choose ~scope:(Tenant |> url_key) specific
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
