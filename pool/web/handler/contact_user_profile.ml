module Command = Cqrs_command.Contact_command
module HttpUtils = Http_utils
module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user
module PoolField = Pool_common.Message.Field

let create_layout = Contact_general.create_layout

let show usage req =
  let result ({ Pool_context.tenant_db; language; _ } as context) =
    let open Utils.Lwt_result.Infix in
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, "/login")
    @@ let* user =
         Http_utils.user_from_session tenant_db req
         ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
       in
       let* contact =
         Contact.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
       in
       match usage with
       | `Overview ->
         Page.Contact.detail contact context
         |> create_layout ~active_navigation:"/user" req context
         >|= Sihl.Web.Response.of_html
       | `LoginInformation ->
         let* password_policy =
           I18n.find_by_key tenant_db I18n.Key.PasswordPolicyText language
         in
         Page.Contact.login_information contact context password_policy
         |> create_layout ~active_navigation:"/user" req context
         >|= Sihl.Web.Response.of_html
       | `PersonalDetails ->
         let* tenant_languages =
           Pool_context.Tenant.find req
           |> Lwt_result.lift
           >|= fun c -> c.Pool_context.Tenant.tenant_languages
         in
         let%lwt custom_fields =
           Custom_field.find_all_by_contact tenant_db (Contact.id contact)
         in
         Page.Contact.personal_details
           Htmx.user_update_csrf
           contact
           custom_fields
           tenant_languages
           context
         |> create_layout req ~active_navigation:"/user" context
         >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let details = show `Overview
let personal_details = show `PersonalDetails
let login_information = show `LoginInformation
let update = Helpers.PartialUpdate.update

let update_email req =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.tenant_db; query_language; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun msg ->
      HttpUtils.(
        msg, "/user/login-information", [ urlencoded_to_flash urlencoded ]))
    @@ let* contact =
         Http_utils.user_from_session tenant_db req
         ||> CCOption.to_result (NotFound Field.User)
         >>= fun user ->
         Contact.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
       in
       let%lwt allowed_email_suffixes =
         let open Utils.Lwt_result.Infix in
         Settings.find_email_suffixes tenant_db
         ||> fun suffixes ->
         if CCList.is_empty suffixes then None else Some suffixes
       in
       let* new_email =
         Pool_user.EmailAddress.create
           (CCList.assoc ~eq:CCString.equal Field.(Email |> show) urlencoded
           |> CCList.hd)
         |> Lwt_result.lift
       in
       let* events =
         Command.RequestEmailValidation.(
           handle ?allowed_email_suffixes contact new_email |> Lwt_result.lift)
       in
       Utils.Database.with_transaction tenant_db (fun () ->
         let%lwt () = Pool_event.handle_events tenant_db events in
         HttpUtils.(
           redirect_to_with_actions
             (path_with_language query_language "/email-confirmation")
             [ Message.set ~success:[ EmailConfirmationMessage ] ]))
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let update_password req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.tenant_db; query_language; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun msg ->
      HttpUtils.(
        msg, "/user/login-information", [ urlencoded_to_flash urlencoded ]))
    @@ let* contact =
         Http_utils.user_from_session tenant_db req
         ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
         >>= fun user ->
         Contact.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
       in
       let* events =
         let open CCResult.Infix in
         Command.UpdatePassword.(decode urlencoded >>= handle contact)
         |> Lwt_result.lift
       in
       Utils.Database.with_transaction tenant_db (fun () ->
         let%lwt () = Pool_event.handle_events tenant_db events in
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
  let open Lwt_result.Syntax in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/login")
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* user =
      Http_utils.user_from_session tenant_db req
      ||> CCOption.to_result Pool_common.Message.(NotFound Field.User)
    in
    let* contact =
      Contact.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
      |> Lwt_result.map_error (fun err -> err)
    in
    let%lwt custom_fields =
      Custom_field.find_all_required_by_contact tenant_db (Contact.id contact)
    in
    Page.Contact.completion context flash_fetcher custom_fields
    |> create_layout req ~active_navigation:"/user" context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let completion_post req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values []
    ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.tenant_db; query_language; _ } =
    Lwt_result.map_error (fun err ->
      HttpUtils.(
        ( err
        , path_with_language query_language "/user/completion"
        , [ urlencoded_to_flash urlencoded ] )))
    @@ let* contact = HttpUtils.get_current_contact tenant_db req in
       let%lwt custom_fields =
         urlencoded
         |> CCList.map (fun pair -> pair |> fst |> Pool_common.Id.of_string)
         |> Custom_field.find_multiple_by_contact tenant_db (Contact.id contact)
       in
       let events =
         custom_fields
         |> CCList.map (fun f ->
              let open CCOption in
              ( CCList.assoc_opt
                  ~eq:CCString.equal
                  Custom_field.(f |> Public.id |> Id.value)
                  urlencoded
                >>= CCList.head_opt
                |> value ~default:""
              , f ))
         |> Cqrs_command.Custom_field_answer_command.UpdateMultiple.handle
              (Contact.id contact)
         |> Lwt_result.lift
       in
       let handle events =
         let%lwt (_ : unit) =
           Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.(
           redirect_to_with_actions
             "/dashboard"
             [ Message.set
                 ~success:[ Pool_common.Message.(Updated Field.Profile) ]
             ])
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;
