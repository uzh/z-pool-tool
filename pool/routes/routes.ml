module CustomMiddleware = Middleware
open Sihl.Web
module Field = Pool_message.Field

let session_expiration = `Max_age (60 * 60 * 4 |> CCInt64.of_int)

let validate_entity action entity =
  CustomMiddleware.Guardian.validate_admin_entity
    Guard.(ValidationSet.one_of_tuple (action, entity, None))
;;

let add_key ?(prefix = "") ?(suffix = "") field =
  let open Field in
  [ prefix; field |> url_key; suffix ]
  |> CCList.filter CCFun.(CCString.is_empty %> not)
  |> CCString.concat "/"
;;

let add_human_field = CCFun.(Field.human_url %> Format.asprintf "/%s")

let global_middlewares =
  [ Middleware.id ~id:(fun () -> CCString.sub (Sihl.Random.base64 12) 0 10) ()
  ; CustomMiddleware.Error.middleware ()
  ; CustomMiddleware.TrailingSlash.middleware ()
  ; Middleware.static_file ()
  ; Opium.Middleware.content_length
  ; Opium.Middleware.etag
  ; Opium.Middleware.method_override
  ]
;;

let web_middlewares =
  [ Middleware.flash ()
  ; Middleware.csrf
      ~not_allowed_handler:CustomMiddleware.NotAllowed.handle
      ~expires:session_expiration
      ()
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

  let middlewares =
    web_middlewares
    @ [ CustomMiddleware.Tenant.validate ()
      ; CustomMiddleware.Context.context ()
      ; CustomMiddleware.Logger.logger
      ]
  ;;

  let routes =
    let locations =
      let open Handler.Contact.Location in
      let specific =
        let files = [ choose ~scope:(add_key Field.File) [ get "" asset ] ] in
        [ get "" show; choose ~scope:"/files" files ]
      in
      [ choose ~scope:(add_key Field.Location) specific ]
    in
    let experiment =
      let open Handler.Contact.Experiment in
      let online_study =
        let open OnlineSurvey in
        [ get (Format.asprintf "submit/%s" Field.(url_key Assignment)) submit ]
      in
      let specific = [ choose online_study ] in
      [ choose ~scope:Field.(url_key Experiment) specific ]
    in
    let queue =
      let open Field in
      let specific =
        [ get "dlr" Handler.Admin.Settings.TextMessages.delivery_report ]
      in
      choose ~scope:(Queue |> url_key) specific
    in
    let announcements =
      let open Handler.Public in
      let specific = [ post "hide" hide_announcement ] in
      choose
        ~scope:Field.(human_url Announcement)
        [ choose ~scope:Field.(url_key Announcement) specific ]
    in
    Handler.Public.(
      choose
        ~middlewares
        [ choose
            ~middlewares:
              [ CustomMiddleware.Guardian.require_user_type_of
                  Pool_context.UserType.[ Guest; Contact; Admin ]
              ]
            [ get "/index" index
            ; get "/custom/assets/index.css" index_css
            ; get "/credits" credits
            ; get "/privacy-policy" privacy_policy
            ; get "/terms-and-conditions" terms_and_conditions
            ; choose ~scope:Field.(Location |> human_url) locations
            ]
        ; choose
            ~middlewares:
              [ CustomMiddleware.Guardian.require_user_type_of
                  Pool_context.UserType.[ Guest ]
              ]
            [ get "/login" Login.login_get
            ; post "/login" Login.login_post
            ; get "/request-reset-password" Login.request_reset_password_get
            ; post "/request-reset-password" Login.request_reset_password_post
            ; get "/reset-password" Login.reset_password_get
            ; post "/reset-password" Login.reset_password_post
            ; get "/import-pending" Import.import_pending
            ; get "/import-confirmation" Import.import_confirmation
            ; post "/import-confirmation" Import.import_confirmation_post
            ; get "/unsubscribe" Import.unsubscribe
            ; post "/unsubscribe" Import.unsubscribe_post
            ]
        ; choose
            ~middlewares:
              [ CustomMiddleware.Guardian.require_user_type_of
                  Pool_context.UserType.[ Guest; Contact ]
              ]
            [ choose ~scope:"/experiments" experiment ]
        ; choose ~scope:"/admin/settings/queue" [ queue ]
        ; choose
            ~middlewares:
              [ CustomMiddleware.Guardian.require_user_type_of
                  Pool_context.UserType.[ Contact; Admin ]
              ]
            [ get "/logout" Login.logout; announcements ]
        ; get "/denied" Handler.Public.denied
        ])
  ;;
end

module Contact = struct
  module Assignment = Handler.Contact.Assignment
  module Experiment = Handler.Contact.Experiment
  module Location = Handler.Contact.Location
  module Session = Handler.Contact.Session
  module SignUp = Handler.Contact.SignUp
  module UserProfile = Handler.Contact.UserProfile
  module WaitingList = Handler.Contact.WaitingList

  let public_not_logged_in =
    [ get "/signup" SignUp.sign_up
    ; post "/signup" SignUp.sign_up_create
    ; get "/email-confirmation" Handler.Public.email_confirmation_note
    ]
  ;;

  let public = [ get "/email-verified" SignUp.email_verification ]

  let locked_routes =
    let locked =
      let experiments =
        let build_scope subdir =
          Format.asprintf "/%s/%s" Field.(Experiment |> url_key) subdir
        in
        let waiting_list =
          [ post "" WaitingList.create; post "/remove" WaitingList.delete ]
        in
        let sessions =
          let open Field in
          [ get (Session |> url_key) Session.show
          ; post (Session |> url_key) Assignment.create
          ]
        in
        let specific =
          [ get "" Experiment.show
          ; get "start" Experiment.OnlineSurvey.redirect
          ]
        in
        [ get "" Experiment.index
        ; choose ~scope:Field.(Experiment |> url_key) specific
        ; choose ~scope:(build_scope "waiting-list") waiting_list
        ; choose ~scope:(build_scope "sessions") sessions
        ]
      in
      [ get "/user/personal-details" UserProfile.personal_details
      ; get "/user/login-information" UserProfile.login_information
      ; get "/user/contact-information" UserProfile.contact_information
      ; get "/user/pause-account" UserProfile.pause_account
      ; post "/user/update" UserProfile.update
      ; post "/user/update/pause" UserProfile.toggle_paused
      ; post "/user/update-email" UserProfile.update_email
      ; post "/user/update-password" UserProfile.update_password
      ; post "/user/phone/update" UserProfile.update_cell_phone
      ; post "/user/phone/verify" UserProfile.verify_cell_phone
      ; post "/user/phone/reset" UserProfile.reset_phone_verification
      ; post "/user/phone/resend-token" UserProfile.resend_token
      ; choose ~scope:"/experiments" experiments
      ]
    in
    [ choose
        ~middlewares:[ CustomMiddleware.Contact.completion_in_progress () ]
        locked
    ; get "/user/completion" UserProfile.completion
    ; post "/user/completion" UserProfile.completion_post
    ]
  ;;

  let middlewares =
    web_middlewares
    @ [ CustomMiddleware.Tenant.validate ()
      ; CustomMiddleware.Context.context ()
      ; CustomMiddleware.Logger.logger
      ]
  ;;

  let routes =
    choose
      ~middlewares
      [ choose
          ~middlewares:
            [ CustomMiddleware.Guardian.require_user_type_of
                Pool_context.UserType.[ Guest ]
            ]
          public_not_logged_in
      ; choose
          ~middlewares:
            [ CustomMiddleware.Guardian.require_user_type_of
                Pool_context.UserType.[ Guest; Contact ]
            ]
          public
      ; choose
          ~middlewares:
            [ CustomMiddleware.Guardian.require_user_type_of
                Pool_context.UserType.[ Contact ]
            ]
          [ choose
              [ get "/accept-terms" SignUp.terms
              ; post "/terms-accepted" SignUp.terms_accept
              ; choose
                  ~middlewares:
                    [ CustomMiddleware.Contact.confirmed_and_terms_agreed () ]
                  locked_routes
              ]
          ]
      ]
  ;;
end

module Admin = struct
  let middlewares =
    web_middlewares
    @ [ CustomMiddleware.Tenant.validate ()
      ; CustomMiddleware.Context.context ()
      ; CustomMiddleware.Logger.logger
      ; CustomMiddleware.Admin.require_admin ()
      ]
  ;;

  let routes =
    let open Field in
    let open Handler.Admin in
    let label_specific_template edit update new_form create delete =
      let specific =
        [ get "/edit" edit; post "" update; post "/delete" delete ]
      in
      [ get "" new_form
      ; post "" create
      ; choose ~scope:(MessageTemplate |> url_key) specific
      ]
    in
    let tag_routes_helper
      (assign_handler, assign_access)
      (remove_handler, remove_access)
      =
      let specific =
        [ post "/remove" ~middlewares:[ remove_access ] remove_handler ]
      in
      [ post "/assign" ~middlewares:[ assign_access ] assign_handler
      ; choose ~scope:(Tag |> url_key) specific
      ]
    in
    let add_template_label label =
      let open Message_template.Label in
      label |> human_url |> Format.asprintf "/%s"
    in
    let location =
      let open Location in
      let files =
        [ get "/create" ~middlewares:[ Access.create_file ] new_file
        ; post "" ~middlewares:[ Access.create_file ] add_file
        ; choose
            ~scope:(add_key File)
            [ get "" ~middlewares:[ Access.read_file ] asset ]
        ]
      in
      let sessions =
        [ get "" ~middlewares:[ Session.Access.read_by_location ] Session.show ]
      in
      let specific =
        [ get "" ~middlewares:[ Access.read ] show
        ; get "/statistics" ~middlewares:[ Access.read ] statistics
        ; get "/changelog" ~middlewares:[ Access.read ] changelog
        ; get "/edit" ~middlewares:[ Access.update ] edit
        ; post "" ~middlewares:[ Access.update ] update
        ; choose ~scope:"/files" files
        ; choose
            ~scope:(add_key ~prefix:"mapping" FileMapping)
            [ post "/delete" ~middlewares:[ Access.delete_file ] delete ]
        ; choose ~scope:(add_key ~prefix:"sessions" Session) sessions
        ]
      in
      [ get "" ~middlewares:[ Access.index ] index
      ; get "/create" ~middlewares:[ Access.create ] new_form
      ; post "" ~middlewares:[ Access.create ] create
      ; post "/search" ~middlewares:[ Access.search ] search
      ; choose ~scope:(add_key Location) specific
      ]
    in
    let filter_form (toggle_key, toggle_predicate_type, add_predicate) =
      [ post "/toggle-key" toggle_key
      ; post "/toggle-predicate-type" toggle_predicate_type
      ; post "/add-predicate" add_predicate
      ]
    in
    let filter =
      let open Handler.Admin.Filter in
      let specific =
        Update.
          [ get "/edit" edit
          ; post "" update_template
          ; get "changelog" changelog
          ; choose
              (filter_form (toggle_key, toggle_predicate_type, add_predicate))
          ]
      in
      Create.
        [ get "" ~middlewares:[ Access.index ] index
        ; post "" ~middlewares:[ Access.create ] create_template
        ; get "/new" ~middlewares:[ Access.create ] new_form
        ; choose
            (filter_form (toggle_key, toggle_predicate_type, add_predicate))
            ~middlewares:[ Access.create ]
        ; choose
            ~middlewares:[ Access.update ]
            ~scope:(Filter |> url_key)
            specific
        ]
    in
    let message_templates =
      let open Handler.Admin.MessageTemplate in
      let specific =
        [ get "/edit" ~middlewares:[ Access.update ] edit
        ; post "" ~middlewares:[ Access.update ] update
        ]
      in
      let label_specific =
        [ get "template-preview" ~middlewares:[ Access.index ] preview_default
        ; post "reset" ~middlewares:[ Access.index ] reset_to_default_htmx
        ]
      in
      [ get "" ~middlewares:[ Access.index ] index
      ; choose ~scope:(url_key Label) label_specific
      ; choose ~scope:(url_key MessageTemplate) specific
      ]
    in
    let experiments =
      let assistants =
        let open Experiments.Users in
        let specific =
          [ post
              "assign"
              ~middlewares:[ Access.assign_assistant ]
              assign_assistant
          ; post
              "unassign"
              ~middlewares:[ Access.unassign_assistant ]
              unassign_assistant
          ]
        in
        [ get "" ~middlewares:[ Access.index_assistants ] index_assistants
        ; choose ~scope:(url_key Admin) specific
        ]
      in
      let experimenter =
        let open Experiments.Users in
        let specific =
          [ post
              "assign"
              ~middlewares:[ Access.assign_experimenter ]
              assign_experimenter
          ; post
              "unassign"
              ~middlewares:[ Access.unassign_experimenter ]
              unassign_experimenter
          ]
        in
        [ get "" ~middlewares:[ Access.index_experimenter ] index_experimenter
        ; choose ~scope:(url_key Admin) specific
        ]
      in
      let invitations =
        Experiments.Invitations.
          [ get "" ~middlewares:[ Access.index ] index
          ; post "" ~middlewares:[ Access.create ] create
          ; get "sent" ~middlewares:[ Access.index ] sent_invitations
          ; post
              (add_key ~suffix:"resend" Invitation)
              ~middlewares:[ Access.resend ]
              resend
          ]
      in
      let sessions =
        let open Session in
        let specific =
          let message_templates =
            let open Message_template.Label in
            let label_specific =
              label_specific_template edit_template update_template
            in
            [ choose
                ~scope:(add_template_label SessionReminder)
                ~middlewares:[ Access.update ]
                (label_specific
                   new_session_reminder
                   new_session_reminder_post
                   delete_message_template)
            ]
          in
          let assignments =
            let open Assignments in
            let specific =
              [ post "/cancel" ~middlewares:[ Access.cancel ] cancel
              ; post "/close" ~middlewares:[ Session.Access.close ] Close.update
              ; post
                  "/verify"
                  ~middlewares:[ Session.Access.close ]
                  Close.verify_contact
              ; get "/edit" ~middlewares:[ Access.update ] edit
              ; post "" ~middlewares:[ Access.update ] update
              ; post "/remind" ~middlewares:[ Access.update ] remind
              ; post
                  "/mark-as-deleted"
                  ~middlewares:[ Access.mark_as_deleted ]
                  mark_as_deleted
              ; choose
                  ~scope:"swap-session"
                  [ get "" ~middlewares:[ Access.update ] swap_session_get
                  ; post "" ~middlewares:[ Access.update ] swap_session_post
                  ; get
                      "template-language"
                      ~middlewares:[ Access.update ]
                      swap_session_toggle_language
                  ]
              ]
            in
            [ choose ~scope:(Assignment |> url_key) specific ]
          in
          let participation_tags =
            let open Handler.Admin.Experiments.Tags in
            tag_routes_helper
              (assign_session_participation_tag, Access.update)
              (remove_session_participation_tag, Access.update)
          in
          let direct_message =
            DirectMessage.
              [ post "" ~middlewares:[ Access.direct_message ] modal_htmx
              ; post "/send" ~middlewares:[ Access.direct_message ] send
              ]
          in
          [ get "" ~middlewares:[ Access.read ] show
          ; post "" ~middlewares:[ Access.update ] update
          ; get "/edit" ~middlewares:[ Access.update ] edit
          ; get "/follow-up" ~middlewares:[ Access.update ] follow_up
          ; post "/follow-up" ~middlewares:[ Access.update ] create_follow_up
          ; get "/cancel" ~middlewares:[ Access.cancel ] cancel_form
          ; post "/cancel" ~middlewares:[ Access.cancel ] cancel
          ; post "/delete" ~middlewares:[ Access.delete ] delete
          ; get "/reschedule" ~middlewares:[ Access.reschedule ] reschedule_form
          ; post "/reschedule" ~middlewares:[ Access.reschedule ] reschedule
          ; get "/close" ~middlewares:[ Access.close ] close
          ; post "/close" ~middlewares:[ Access.close ] close_post
          ; get "/print" ~middlewares:[ Access.read ] print
          ; get "/changelog" ~middlewares:[ Access.read ] changelog
          ; post
              "/update-matches-filter"
              ~middlewares:[ Access.update_matches_filter ]
              update_matches_filter
          ; get "/duplicate" ~middlewares:[ Access.create ] duplicate
          ; post "/duplicate" ~middlewares:[ Access.create ] duplicate_post_htmx
          ; get
              "/duplicate/form"
              ~middlewares:[ Access.create ]
              duplicate_form_htmx
          ; post
              "/resend-reminders"
              ~middlewares:[ Access.update ]
              resend_reminders
          ; post
              "/toggle-assignments"
              ~middlewares:[ Session.Access.close ]
              Assignments.Close.toggle
          ; choose ~scope:(add_human_field Assignments) assignments
          ; choose ~scope:(add_human_field MessageTemplate) message_templates
          ; choose ~scope:(ParticipationTag |> human_url) participation_tags
          ; choose ~scope:"direct-message" direct_message
          ]
        in
        [ get "" ~middlewares:[ Access.index ] list
        ; get "/create" ~middlewares:[ Access.create ] new_form
        ; post "" ~middlewares:[ Access.create ] create
        ; choose ~scope:"/:session" specific
        ]
      in
      let waiting_list =
        let open Experiments.WaitingList in
        let specific =
          [ get "" ~middlewares:[ Access.read ] detail
          ; post "" ~middlewares:[ Access.update ] update
          ; post "/assign" ~middlewares:[ Access.assign ] assign_contact
          ]
        in
        [ get "" ~middlewares:[ Access.index ] index
        ; choose ~scope:(WaitingList |> url_key) specific
        ]
      in
      let mailings =
        let open Experiments.Mailings in
        let specific =
          [ get "" ~middlewares:[ Access.read ] show
          ; post "" ~middlewares:[ Access.update ] update
          ; get "/edit" ~middlewares:[ Access.update ] edit
          ; post "/stop" ~middlewares:[ Access.stop ] stop
          ; post "/delete" ~middlewares:[ Access.delete ] delete
          ]
        in
        [ get "" ~middlewares:[ Access.index ] index
        ; post "" ~middlewares:[ Access.create ] create
        ; get "/create" ~middlewares:[ Access.create ] new_form
        ; post "/search-info" ~middlewares:[ Access.search_info ] search_info
        ; post
            "/add-condition"
            ~middlewares:[ Access.add_condition ]
            add_condition
        ; choose ~scope:(Mailing |> url_key) specific
        ]
      in
      let filter =
        let open Handler.Admin.Experiments in
        let form_handlers middlewares =
          choose
            (filter_form
               Experiments.Filter.(
                 toggle_key, toggle_predicate_type, add_predicate))
            ~middlewares
        in
        let specific =
          [ post "" ~middlewares:[ Access.Filter.update ] Filter.update
          ; post "/delete" ~middlewares:[ Access.Filter.delete ] Filter.delete
          ; form_handlers [ Access.Filter.update ]
          ]
        in
        [ post "/create" ~middlewares:[ Access.Filter.create ] Filter.create
        ; form_handlers [ Access.Filter.create ]
        ; choose ~scope:(Filter |> url_key) specific
        ]
      in
      let message_templates =
        let open Handler.Admin.Experiments.MessageTemplates in
        let label_specific =
          label_specific_template edit_template update_template
        in
        [ choose
            ~scope:(Label |> url_key)
            ~middlewares:[ Access.message_template ]
            (label_specific
               new_message_template
               new_message_template_post
               delete)
        ]
      in
      let tags =
        let open Handler.Admin.Settings.Tags in
        let open Handler.Admin.Experiments.Tags in
        tag_routes_helper
          (assign_tag, Access.assign_tag_to_experiment)
          (remove_tag, Access.remove_tag_from_experiment)
      in
      let participation_tags =
        let open Handler.Admin.Experiments in
        tag_routes_helper
          (Tags.assign_experiment_participation_tag, Access.update)
          (Tags.remove_experiment_participation_tag, Access.update)
      in
      let specific =
        Experiments.
          [ get "" ~middlewares:[ Access.read ] show
          ; post "" ~middlewares:[ Access.update ] update
          ; get "/edit" ~middlewares:[ Access.update ] edit
          ; post "/delete" ~middlewares:[ Access.delete ] delete
          ; get "/changelog" ~middlewares:[ Access.read ] changelog
          ; post
              "/reset-invitations"
              ~middlewares:[ Access.update ]
              Invitations.reset
          ; post
              "/contact-count"
              ~middlewares:[ Access.read ]
              Handler.Admin.Filter.count_contacts
          ; get
              "/messages"
              ~middlewares:[ Access.message_history ]
              message_history
          ; get
              (Format.asprintf "/contact-history/%s" (add_key Field.Contact))
              ~middlewares:[ Contacts.Access.read ]
              Contacts.experiment_history
          ; choose ~scope:"/assistants" assistants
          ; choose ~scope:"/experimenter" experimenter
          ; choose ~scope:"/invitations" invitations
          ; choose ~scope:"/waiting-list" waiting_list
          ; choose ~scope:"/sessions" sessions
          ; choose ~scope:"/mailings" mailings
          ; choose ~scope:"/filter" filter
          ; choose ~scope:(Tag |> human_url) tags
          ; choose ~scope:(ParticipationTag |> human_url) participation_tags
          ; choose ~scope:(add_human_field MessageTemplate) message_templates
          ]
      in
      Experiments.
        [ get "" ~middlewares:[ Access.index ] index
        ; post "" ~middlewares:[ Access.create ] create
        ; get "/create" ~middlewares:[ Access.create ] new_form
        ; post "/search" ~middlewares:[ Access.search ] search
        ; choose ~scope:(add_key Experiment) specific
        ]
    in
    let sessions =
      let open Session in
      [ get "" Api.current_user
      ; get
          Field.(
            Format.asprintf "%s/%s" (human_url Location) (url_key Location))
          ~middlewares:[ Api.Access.location ]
          Api.location
      ]
    in
    let admins =
      let open Handler.Admin.Admin in
      let specific =
        [ get "" ~middlewares:[ Access.read ] detail
        ; get "/edit" ~middlewares:[ Access.update ] edit
        ; post "/toggle-role" ~middlewares:[ Access.read ] handle_toggle_role
        ; post
            "/search-role"
            ~middlewares:[ Access.grant_role ]
            search_role_entities
        ; post "/grant-role" ~middlewares:[ Access.grant_role ] grant_role
        ; post "/revoke-role" ~middlewares:[ Access.revoke_role ] revoke_role
        ]
      in
      [ get "" ~middlewares:[ Access.index ] index
      ; post "" ~middlewares:[ Access.create ] create_admin
      ; get "/new" ~middlewares:[ Access.create ] new_form
      ; post "/search" ~middlewares:[ Access.search ] search
      ; choose ~scope:(Admin |> url_key) specific
      ]
    in
    let contacts =
      let open Contacts in
      let duplicates =
        let open Duplicates in
        let specific =
          [ get "" ~middlewares:[ Access.read ] show
          ; post "ignore" ~middlewares:[ Access.ignore ] ignore
          ; post "merge" ~middlewares:[ Access.merge ] merge
          ]
        in
        [ get "" ~middlewares:[ Access.index ] index
        ; choose ~scope:(Duplicate |> url_key) specific
        ]
      in
      let specific =
        let field_specific =
          [ post "/delete" ~middlewares:[ Access.update ] delete_answer ]
        in
        let tags =
          let open Settings.Tags in
          tag_routes_helper
            (Tags.assign_tag, Access.assign_tag_to_contact)
            (Tags.remove_tag, Access.remove_tag_from_contact)
        in
        let experiment =
          [ get "" htmx_experiments_get
          ; get (Experiment |> url_key) htmx_experiment_modal
          ; post (Experiment |> url_key) enroll_contact_post
          ]
        in
        let duplicates =
          let open Duplicates in
          [ get "" ~middlewares:[ Access.index ] index ]
        in
        [ get "" ~middlewares:[ Access.read ] detail
        ; post "" ~middlewares:[ Access.update ] update
        ; post "pause" ~middlewares:[ Access.update ] toggle_paused
        ; post "verify" ~middlewares:[ Access.update ] toggle_verified
        ; post "delete" ~middlewares:[ Access.update ] mark_as_deleted
        ; get "/edit" ~middlewares:[ Access.update ] edit
        ; post "/promote" ~middlewares:[ Access.promote ] promote
        ; get "/changelog" ~middlewares:[ Access.changelog ] changelog
        ; get
            "/past-experiments"
            ~middlewares:[ Access.read ]
            past_experiments_htmx
        ; get
            "/messages"
            ~middlewares:[ Access.message_history ]
            message_history
        ; choose
            ~middlewares:[ Access.update ]
            ~scope:(Experiments |> human_url)
            experiment
        ; get
            (ExternalDataId |> human_url)
            ~middlewares:[ Access.read ]
            external_data_ids
        ; choose
            ~scope:(Format.asprintf "field/%s" (CustomField |> url_key))
            field_specific
        ; choose ~scope:(Tag |> human_url) tags
        ; choose ~scope:(Duplicate |> human_url) duplicates
        ]
      in
      [ get "" ~middlewares:[ Access.index ] index
      ; choose ~scope:(Duplicate |> human_url) duplicates
      ; choose ~scope:(Contact |> url_key) specific
      ]
    in
    let users =
      let open Handler.Admin.Users in
      let specific = [ get "" redirect ] in
      [ choose ~scope:(User |> url_key) specific ]
    in
    let custom_fields =
      let open CustomField in
      let specific =
        let options =
          let open CustomFieldOption in
          let specific =
            [ get "/edit" ~middlewares:[ Access.update ] edit
            ; post "" ~middlewares:[ Access.update ] update
            ; post "/delete" ~middlewares:[ Access.delete ] delete
            ; post "/publish" ~middlewares:[ Access.publish ] publish
            ; get "/changelog" ~middlewares:[ Access.update ] changelog
            ]
          in
          [ get "/new" ~middlewares:[ Access.create ] new_form
          ; post "" ~middlewares:[ Access.create ] create
          ; choose ~scope:(CustomFieldOption |> url_key) specific
          ]
        in
        [ post "" ~middlewares:[ Access.update ] update
        ; get "/edit" ~middlewares:[ Access.update ] edit
        ; post "/publish" ~middlewares:[ Access.publish ] publish
        ; post "/delete" ~middlewares:[ Access.delete ] delete
        ; post "/sort-options" ~middlewares:[ Access.update ] sort_options
        ; get "/changelog" ~middlewares:[ Access.update ] changelog
        ; choose ~scope:"options" options
        ]
      in
      let fields =
        [ post "" ~middlewares:[ Access.create ] create
        ; get "/new" ~middlewares:[ Access.create ] new_form
        ; choose ~scope:(CustomField |> url_key) specific
        ]
      in
      let groups =
        let open CustomFieldGroup in
        let specific =
          [ get "/edit" ~middlewares:[ Access.update ] edit
          ; post "" ~middlewares:[ Access.update ] update
          ; post "/delete" ~middlewares:[ Access.delete ] delete
          ; post "sort-fields" ~middlewares:[ Access.sort_fields ] sort_fields
          ; get "/changelog" ~middlewares:[ Access.update ] changelog
          ]
        in
        [ get "/new" ~middlewares:[ Access.create ] new_form
        ; post "" ~middlewares:[ Access.create ] create
        ; post "/sort" ~middlewares:[ Access.sort ] sort
        ; choose ~scope:(CustomFieldGroup |> url_key) specific
        ]
      in
      let models =
        [ get "" ~middlewares:[ Access.index ] index
        ; post "sort-fields" ~middlewares:[ Access.sort ] sort_ungrouped_fields
        ; choose ~scope:"field" fields
        ; choose ~scope:"group" groups
        ]
      in
      let settings =
        let open CustomFieldSettings in
        [ get "" ~middlewares:[ Access.index ] index
        ; post
            "session-close"
            ~middlewares:[ Access.update ]
            update_close_screen
        ; post
            "session-detail"
            ~middlewares:[ Access.update ]
            update_detail_screen
        ]
      in
      [ get "" ~middlewares:[ Access.index ] redirect
      ; choose ~scope:"settings" settings
      ; choose ~scope:(Model |> url_key) models
      ]
    in
    let i18n =
      let open I18n in
      [ get "" ~middlewares:[ Access.index ] index
      ; post
          (Format.asprintf "/%s" (I18n |> url_key))
          ~middlewares:[ Access.update ]
          update
      ]
    in
    let organisational_units =
      let open OrganisationalUnit in
      let specific =
        [ get "/edit" ~middlewares:[ Access.update ] edit
        ; post "" ~middlewares:[ Access.update ] update
        ; get "/changelog" ~middlewares:[ Access.index ] changelog
        ]
      in
      [ get "" ~middlewares:[ Access.index ] index
      ; get "create" ~middlewares:[ Access.create ] new_form
      ; post "" ~middlewares:[ Access.create ] create
      ; choose ~scope:(OrganisationalUnit |> url_key) specific
      ]
    in
    let settings =
      let open Settings in
      let queue =
        let open Queue in
        let specific =
          [ get "" ~middlewares:[ Access.read ] detail
          ; post "resend" ~middlewares:[ Access.resend ] resend
          ]
        in
        [ get "" ~middlewares:[ Access.index ] show
        ; get "/archive" ~middlewares:[ Access.index ] show_archive
        ; choose ~scope:(Queue |> url_key) specific
        ]
      in
      let actor_permission =
        let open ActorPermission in
        [ get "" ~middlewares:[ Access.index ] show
        ; get "/new" ~middlewares:[ Access.create ] new_form
        ; post "" ~middlewares:[ Access.create ] create
        ; post "remove" ~middlewares:[ Access.delete ] delete
        ; post
            "/toggle-target"
            ~middlewares:[ Access.create ]
            handle_toggle_target
        ]
      in
      let role_permission =
        let open RolePermission in
        let specific =
          let target_specific =
            [ get "edit" ~middlewares:[ Access.read ] edit_htmx
            ; post "" ~middlewares:[ Access.read ] update
            ]
          in
          [ get "" ~middlewares:[ Access.read ] show
          ; choose
              ~scope:(Format.asprintf "target/%s" (url_key Target))
              target_specific
          ]
        in
        [ get "" ~middlewares:[ Access.read ] index
        ; choose ~scope:(url_key Role) specific
        ]
      in
      let api_key =
        let open ApiKey in
        let specific =
          [ get "" ~middlewares:[ Access.read ] show
          ; get "edit" ~middlewares:[ Access.update ] edit
          ; post "" ~middlewares:[ Access.update ] update
          ; post "/disable" ~middlewares:[ Access.disable ] disable
          ; post "/toggle-role" ~middlewares:[ Access.read ] handle_toggle_role
          ; post
              "/search-role"
              ~middlewares:[ Access.grant_role ]
              search_role_entities
          ; post "/grant-role" ~middlewares:[ Access.grant_role ] grant_role
          ; post "/revoke-role" ~middlewares:[ Access.revoke_role ] revoke_role
          ]
        in
        [ get "" ~middlewares:[ Access.index ] index
        ; post "" ~middlewares:[ Access.create ] create
        ; get "/new" ~middlewares:[ Access.create ] new_form
        ; choose ~scope:(ApiKey |> url_key) specific
        ]
      in
      let smtp =
        let open Smtp in
        let specific =
          [ get "" ~middlewares:[ Access.update ] show
          ; post "" ~middlewares:[ Access.update ] update
          ; post "/delete" ~middlewares:[ Access.delete ] delete
          ; post "/password" ~middlewares:[ Access.update ] update_password
          ; post "/validate" ~middlewares:[ Access.validate ] validate_tenant
          ]
        in
        [ get "" ~middlewares:[ Access.index ] index
        ; get "/new" ~middlewares:[ Access.create ] new_form
        ; post "/create" ~middlewares:[ Access.create ] create
        ; choose ~scope:(Smtp |> url_key) specific
        ]
      in
      let tags =
        let open Tags in
        let specific =
          [ get "" ~middlewares:[ Access.update ] edit
          ; post "" ~middlewares:[ Access.update ] update
          ]
        in
        [ get "" ~middlewares:[ Access.index ] index
        ; get "/create" ~middlewares:[ Access.create ] new_form
        ; post "" ~middlewares:[ Access.create ] create
        ; post "/search" ~middlewares:[ Access.search ] search
        ; choose ~scope:(Tag |> url_key) specific
        ]
      in
      let text_messages =
        let open TextMessages in
        [ get "" ~middlewares:[ Access.index ] index
        ; post "" ~middlewares:[ Access.update ] update
        ; post "delete" ~middlewares:[ Access.delete ] delete
        ]
      in
      let signup_codes =
        let open SignupCodes in
        [ get "" ~middlewares:[ Access.index ] index ]
      in
      [ get "" ~middlewares:[ Access.index ] show
      ; choose ~scope:"/queue" queue
      ; choose ~scope:"/actor-permission" actor_permission
      ; choose ~scope:"/role-permission" role_permission
      ; choose ~scope:Field.(human_url ApiKey) api_key
      ; choose ~scope:"/smtp" smtp
      ; choose ~scope:"/tags" tags
      ; choose ~scope:"/text-messages" text_messages
      ; choose ~scope:Field.(human_url SignUpCode) signup_codes
      ; post "/:action" ~middlewares:[ Access.update ] update_settings
      ; get "/schedules" ~middlewares:[ Schedule.Access.index ] Schedule.show
      ]
    in
    let versions =
      let open Version in
      let specific = [ get "" show ] in
      [ get "" index; choose ~scope:Field.(url_key Version) specific ]
    in
    let profile =
      let open Profile in
      [ get "/login-information" show
      ; post "/update-details" update_name
      ; post "/update-password" update_password
      ]
    in
    let dashboard =
      Dashboard.
        [ get "/dashboard" index
        ; get "/dashboard/upcoming-sessions" upcoming_sessions
        ; get "/dashboard/incomplete-sessions" incomplete_sessions
        ; get "/statistics" ~middlewares:[ Access.Statistics.read ] statistics
        ]
    in
    choose
      ~middlewares
      [ choose dashboard
      ; choose ~scope:"/settings" settings
      ; choose ~scope:"/user" profile
      ; choose ~scope:"/i18n" i18n
      ; choose ~scope:"/experiments" experiments
      ; choose ~scope:"/filter" filter
      ; choose ~scope:"/locations" location
      ; choose ~scope:"/contacts" contacts
      ; choose ~scope:"/admins" admins
      ; choose ~scope:"/users" users
      ; choose ~scope:"/custom-fields" custom_fields
      ; choose ~scope:(add_human_field Field.Sessions) sessions
      ; choose ~scope:(add_human_field OrganisationalUnit) organisational_units
      ; choose ~scope:(add_human_field MessageTemplate) message_templates
      ; choose ~scope:(add_human_field Version) versions
      ]
  ;;
end

module Root = struct
  let middlewares =
    web_middlewares
    @ [ CustomMiddleware.Root.from_root_only ()
      ; CustomMiddleware.Context.context ()
      ; CustomMiddleware.Logger.logger
      ]
  ;;

  let public_routes =
    let open Handler.Root in
    [ get "/login" Login.login_get
    ; post "/login" Login.login_post
    ; get "/request-reset-password" Login.request_reset_password_get
    ; post "/request-reset-password" Login.request_reset_password_post
    ; get "/reset-password" Login.reset_password_get
    ; post "/reset-password" Login.reset_password_post
    ; get "/api/v1/status" Status.show
    ]
  ;;

  let locked_middlewares = [ CustomMiddleware.Root.require_root () ]

  let locked_routes =
    let open Field in
    let open Handler.Root in
    let tenants =
      let open Tenant in
      let specific =
        [ get "" ~middlewares:[ Access.read ] tenant_detail
        ; get ~middlewares:[ Access.read_operator ] "operator" manage_operators
        ; post
            "/create-operator"
            ~middlewares:[ Access.create_operator ]
            create_operator
        ; post
            "/update-detail"
            ~middlewares:[ Access.update ]
            Update.update_detail
        ; post
            "/update-database"
            ~middlewares:[ Access.update ]
            Update.update_database
        ; post
            (Format.asprintf "/assets/%s/delete" (AssetId |> url_key))
            ~middlewares:[ Access.update ]
            Tenant.Update.delete_asset
        ]
      in
      [ get "" ~middlewares:[ Access.index ] Tenant.tenants
      ; post "/create" ~middlewares:[ Access.create ] Tenant.create
      ; choose ~scope:(Tenant |> url_key) specific
      ]
    in
    let users =
      let open Users in
      let specific =
        [ post
            "/toggle-status"
            ~middlewares:[ Access.toggle_status ]
            toggle_status
        ]
      in
      [ get "" ~middlewares:[ Access.index ] index
      ; post "/create" ~middlewares:[ Access.create ] create
      ; choose ~scope:(Root |> url_key) specific
      ]
    in
    let announcements =
      let open Announcement in
      let specific = [ post "" update; get "edit" edit ] in
      [ get "" index
      ; post "" create
      ; get "new" new_form
      ; choose ~scope:Field.(url_key Announcement) specific
      ]
    in
    let versions =
      let open Version in
      let specific =
        [ post "" update; get "edit" edit; post "publish" publish ]
      in
      [ get "" index
      ; post "" create
      ; get "new" new_form
      ; choose ~scope:Field.(url_key Version) specific
      ]
    in
    let settings =
      let smtp =
        let open Handler.Root.Settings in
        [ get "" ~middlewares:[ Access.index ] show_smtp
        ; post "/create" ~middlewares:[ Access.create ] create_smtp
        ; post "/:smtp" ~middlewares:[ Access.update ] update_smtp
        ; post "/:smtp/delete" ~middlewares:[ Access.delete ] delete_smtp
        ; post
            "/:smtp/password"
            ~middlewares:[ Access.update ]
            update_smtp_password
        ; post "/:smtp/validate" ~middlewares:[ Access.validate ] validate
        ]
      in
      [ choose ~scope:"/smtp" smtp ]
    in
    let profile =
      let open Profile in
      [ get "/login-information" show
      ; post "/update-details" update_name
      ; post "/update-password" update_password
      ]
    in
    [ choose
        [ get "/logout" Login.logout
        ; choose ~scope:Field.(show Announcement) announcements
        ; choose ~scope:Field.(show Version) versions
        ; choose ~scope:"/settings" settings
        ; choose ~scope:"/user" profile
        ; choose ~scope:"/tenants" tenants
        ; choose ~scope:"/users" users
        ]
    ]
  ;;

  let routes =
    let open Handler in
    choose
      ~middlewares
      [ get "" Root.forward_to_entrypoint
      ; choose
          ~middlewares:
            [ CustomMiddleware.Guardian.require_user_type_of
                Pool_context.UserType.[ Guest ]
            ]
          public_routes
      ; get "/denied" Public.denied
      ; choose ~middlewares:locked_middlewares locked_routes
      ]
  ;;
end

module Api = struct
  open Api

  let global_middlewares =
    CustomMiddleware.[ Api.api_request (); Api.context (); Logger.logger ]
  ;;

  module V1 = struct
    open V1

    let experiment =
      let open Experiment in
      let specific = [ get "" ~middlewares:[ Access.read ] show ] in
      choose
        ~scope:Field.(human_url Experiment)
        [ get "" ~middlewares:[ Access.index ] index
        ; choose ~scope:Field.(url_key Experiment) specific
        ]
    ;;

    let organisational_unit =
      let open OrganisationalUnit in
      choose ~scope:Field.(human_url OrganisationalUnit) [ get "" index ]
    ;;

    let routes =
      choose
        [ experiment
        ; organisational_unit
        ; get "/**" ~middlewares:global_middlewares not_found
        ]
    ;;
  end

  let routes =
    choose
      ~middlewares:
        (CustomMiddleware.Api.validate_tenant () :: global_middlewares)
      [ choose ~scope:"/v1" [ V1.routes ]
      ; get "/**" ~middlewares:global_middlewares not_found
      ]
  ;;
end

let router =
  choose
    [ Public.routes
    ; Contact.routes
    ; choose ~scope:"/admin" [ Admin.routes ]
    ; choose ~scope:"/root" [ Root.routes ]
    ; choose ~scope:"/api" [ Api.routes ]
    ; Public.global_routes
    ; get
        "/**"
        ~middlewares:
          [ CustomMiddleware.Context.context ()
          ; CustomMiddleware.Logger.logger
          ]
        Handler.Public.not_found
    ]
;;
