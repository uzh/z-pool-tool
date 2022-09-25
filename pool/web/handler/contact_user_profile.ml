module Command = Cqrs_command.Contact_command
module HttpUtils = Http_utils
module Conformist = Pool_common.Utils.PoolConformist
module User = Pool_user
module PoolField = Pool_common.Message.Field

let create_layout = Contact_general.create_layout
let user_update_csrf = "_user_update_csrf"

let parse_urlencoded urlencoded =
  let open Pool_common.Message in
  let open CCResult in
  let find_param name err =
    let open CCOption in
    CCList.assoc_opt ~eq:CCString.equal name urlencoded
    >>= CCList.head_opt
    |> to_result err
  in
  let* field_str = find_param "field" InvalidHtmxRequest in
  let field =
    try PoolField.read field_str with
    (* TODO: Check if given field is custom field name *)
    | _ -> PoolField.CustomHtmx (field_str, field_str)
  in
  let* version =
    find_param "version" (HtmxVersionNotFound field_str)
    >>= fun i ->
    i
    |> CCInt.of_string
    |> CCOption.map Pool_common.Version.of_int
    |> CCOption.to_result Pool_common.Message.(Invalid Field.Version)
  in
  let* value = find_param field_str InvalidHtmxRequest in
  Ok (field, version, value)
;;

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
         |> Lwt_result.map_error (fun err -> err)
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
           Custom_field.find_all_for_contact tenant_db (Contact.id contact)
         in
         Page.Contact.personal_details
           user_update_csrf
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

let update req =
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_htmx_request_boolean_values Field.[ Paused |> show ]
  in
  let result { Pool_context.csrf; tenant_db; language; query_language; _ } =
    let path_with_lang = HttpUtils.path_with_language query_language in
    let with_redirect path res =
      res |> CCResult.map_err (fun err -> err, path_with_lang path)
    in
    let open Utils.Lwt_result.Syntax in
    let* user =
      Http_utils.user_from_session tenant_db req
      ||> CCOption.to_result (NotFound Field.User)
      ||> with_redirect "/login"
    in
    let* contact =
      Contact.find tenant_db (user.Sihl_user.id |> Pool_common.Id.of_string)
      ||> with_redirect "/login"
    in
    let* Pool_context.Tenant.{ tenant_languages; _ } =
      Pool_context.Tenant.find req
      |> with_redirect "/user/personal-details"
      |> Lwt_result.lift
    in
    let* field, version, value =
      parse_urlencoded urlencoded
      |> with_redirect "/user/personal-details"
      |> Lwt_result.lift
    in
    let%lwt response =
      let open CCResult in
      let partial_update =
        Contact.validate_partial_update contact (field, version, value)
      in
      let events =
        let open CCResult in
        partial_update >>= Cqrs_command.Contact_command.Update.handle contact
      in
      let htmx_element () =
        let hx_post =
          Sihl.Web.externalize_path (path_with_lang "/user/update")
        in
        let csrf_element =
          Htmx.csrf_element_swap csrf ~id:user_update_csrf ()
        in
        let htmx =
          let open Pool_common.Message in
          match partial_update with
          | Ok partial_update ->
            let input =
              partial_update
              |> Contact.PartialUpdate.increment_version
              |> Htmx.partial_update_to_htmx tenant_languages
            in
            Htmx.create input language ~hx_post ~success:true ()
          | Error error ->
            let[@warning "-4"] value =
              match field with
              | Field.Firstname -> Htmx.Text (value |> CCOption.pure)
              | Field.Lastname -> Htmx.Text (value |> CCOption.pure)
              | Field.Paused -> Htmx.Checkbox false
              | Field.Language ->
                let open Htmx in
                Select
                  { show = Pool_common.Language.show
                  ; options = tenant_languages
                  ; selected =
                      value |> Pool_common.Language.create |> CCResult.to_opt
                  }
              | _ -> failwith "TODO"
            in
            Htmx.create (version, field, value) language ~hx_post ~error ()
        in
        [ htmx; csrf_element ] |> HttpUtils.multi_html_to_plain_text_response
      in
      (* TODO: When and where to update version? *)
      let%lwt () =
        match events with
        | Error _ -> Lwt.return_unit
        | Ok events ->
          events |> Lwt_list.iter_s (Pool_event.handle_event tenant_db)
      in
      htmx_element () |> Lwt.return
    in
    response |> Lwt_result.return
  in
  HttpUtils.extract_happy_path_htmx req result
;;

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
