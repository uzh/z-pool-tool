module Command = Cqrs_command.Contact_command
module HttpUtils = Http_utils
module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user
module PoolField = Pool_common.Message.Field

let create_layout = Contact_general.create_layout

let show usage req =
  let result ({ Pool_context.database_label; language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err -> err, "/login")
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       match usage with
       | `LoginInformation ->
         let* password_policy =
           I18n.find_by_key database_label I18n.Key.PasswordPolicyText language
         in
         Page.Contact.login_information contact context password_policy
         |> create_layout
              ~active_navigation:"/user/login-information"
              req
              context
         >|+ Sihl.Web.Response.of_html
       | `PersonalDetails ->
         let* tenant_languages =
           Pool_context.Tenant.find req
           |> Lwt_result.lift
           >|+ fun c -> c.Pool_context.Tenant.tenant_languages
         in
         let%lwt custom_fields =
           Custom_field.find_all_by_contact database_label (Contact.id contact)
         in
         Page.Contact.personal_details
           contact
           custom_fields
           tenant_languages
           context
         |> create_layout
              req
              ~active_navigation:"/user/personal-details"
              context
         >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let personal_details = show `PersonalDetails
let login_information = show `LoginInformation
let update = Helpers.PartialUpdate.update

let update_email req =
  let open Pool_common.Message in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result ({ Pool_context.database_label; query_language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun msg ->
      HttpUtils.(
        msg, "/user/login-information", [ urlencoded_to_flash urlencoded ]))
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let%lwt allowed_email_suffixes =
         let open Utils.Lwt_result.Infix in
         Settings.find_email_suffixes database_label
         ||> fun suffixes ->
         if CCList.is_empty suffixes then None else Some suffixes
       in
       let* new_email =
         Pool_user.EmailAddress.create
           (CCList.assoc ~eq:CCString.equal Field.(Email |> show) urlencoded
           |> CCList.hd)
         |> Lwt_result.lift
       in
       let* { Pool_context.Tenant.tenant; _ } =
         Pool_context.Tenant.find req |> Lwt_result.lift
       in
       let* events =
         Command.RequestEmailValidation.(
           handle ?allowed_email_suffixes tenant contact new_email
           |> Lwt_result.lift)
       in
       Utils.Database.with_transaction database_label (fun () ->
         let tags = Logger.req req in
         let%lwt () = Pool_event.handle_events ~tags database_label events in
         HttpUtils.(
           redirect_to_with_actions
             (path_with_language query_language "/email-confirmation")
             [ Message.set ~success:[ EmailConfirmationMessage ] ]))
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let update_password req =
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result ({ Pool_context.database_label; query_language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    let tags = Logger.req req in
    Utils.Lwt_result.map_error (fun msg ->
      HttpUtils.(
        msg, "/user/login-information", [ urlencoded_to_flash urlencoded ]))
    @@ let* contact = Pool_context.find_contact context |> Lwt_result.lift in
       let* { Pool_context.Tenant.tenant; _ } =
         Pool_context.Tenant.find req |> Lwt_result.lift
       in
       let* events =
         let open CCResult.Infix in
         Command.UpdatePassword.(
           decode urlencoded >>= handle ~tags tenant contact)
         |> Lwt_result.lift
       in
       Utils.Database.with_transaction database_label (fun () ->
         let%lwt () = Pool_event.handle_events ~tags database_label events in
         HttpUtils.(
           redirect_to_with_actions
             (path_with_language query_language "/user/login-information")
             [ Message.set ~success:[ Pool_common.Message.PasswordChanged ] ]))
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let completion req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/login")
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* contact = Pool_context.find_contact context |> Lwt_result.lift in
    let%lwt custom_fields =
      Custom_field.find_all_required_by_contact
        database_label
        (Contact.id contact)
    in
    Page.Contact.completion context flash_fetcher custom_fields
    |> create_layout req ~active_navigation:"/user" context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let completion_post req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values []
    ||> HttpUtils.remove_empty_values
  in
  let result
    ({ Pool_context.database_label; query_language; language; _ } as context)
    =
    Utils.Lwt_result.map_error (fun err ->
      HttpUtils.(
        ( err
        , path_with_language query_language "/user/completion"
        , [ urlencoded_to_flash urlencoded ] )))
    @@
    let open Custom_field in
    let* contact = Pool_context.find_contact context |> Lwt_result.lift in
    let%lwt custom_fields =
      urlencoded
      |> CCList.map (fun pair -> pair |> fst |> Pool_common.Id.of_string)
      |> Custom_field.find_multiple_by_contact
           database_label
           (Contact.id contact)
    in
    let tags = Logger.req req in
    let events =
      let open Utils.Lwt_result.Infix in
      let open Public in
      let handle =
        Cqrs_command.Custom_field_answer_command.UpdateMultiple.handle
          ~tags
          (Contact.id contact)
      in
      Lwt_list.map_s
        (fun field ->
          let id = field |> Public.id |> Id.value in
          (match field with
           | MultiSelect _ ->
             req
             |> HttpUtils.htmx_urlencoded_list
                  (field
                  |> Public.to_common_field language
                  |> Pool_common.Message.Field.array_key)
           | Boolean _ | Number _ | Select _ | Text _ ->
             CCList.assoc_opt ~eq:CCString.equal id urlencoded
             |> CCOption.value ~default:[]
             |> Lwt.return)
          ||> CCFun.flip validate_htmx field
          >>= fun field -> handle field |> Lwt_result.lift)
        custom_fields
      ||> CCList.all_ok
    in
    let handle events =
      let%lwt (_ : unit list) =
        Lwt_list.map_s (Pool_event.handle_event ~tags database_label) events
      in
      let%lwt required_answers_given =
        Custom_field.all_required_answered database_label (Contact.id contact)
      in
      match required_answers_given with
      | true ->
        Http_utils.(
          redirect_to_with_actions
            "/dashboard"
            [ Message.set
                ~success:[ Pool_common.Message.(Updated Field.Profile) ]
            ; Sihl.Web.Session.set [ Contact.profile_completion_cookie, "" ]
            ])
      | false ->
        Http_utils.(
          redirect_to_with_actions
            "/user/completion"
            [ Message.set ~error:[ Pool_common.Message.(RequiredFieldsMissing) ]
            ])
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;
