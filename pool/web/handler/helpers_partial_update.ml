open Pool_message
module HttpUtils = Http_utils

let src = Logs.Src.create "handler.helper.partial_update"

let parse_urlencoded ~is_admin req database_label language urlencoded contact_id
  =
  let open Error in
  let open Utils.Lwt_result.Infix in
  let find_param_list name =
    CCList.assoc_opt ~eq:CCString.equal name urlencoded
  in
  let find_param_opt name =
    let open CCOption in
    name |> find_param_list >>= CCList.head_opt
  in
  let find_param name err =
    name |> find_param_opt |> CCOption.to_result err |> Lwt_result.lift
  in
  let field_id =
    Htmx.field_id_key
    |> find_param_opt
    |> CCOption.map Custom_field.Id.of_string
  in
  let* field_str = find_param "field" InvalidHtmxRequest in
  let* field =
    let parsed =
      try Ok (Field.read field_str) with
      | _ -> Error InvalidHtmxRequest
    in
    match parsed with
    | Ok field -> field |> Lwt.return_ok
    | Error _ ->
      field_id
      |> CCOption.to_result InvalidHtmxRequest
      |> Lwt_result.lift
      >>= Custom_field.find_by_contact ~is_admin database_label contact_id
      >|+ fun f -> Custom_field.Public.to_common_field language f
  in
  let* version =
    find_param "version" (HtmxVersionNotFound field_str)
    >>= fun i ->
    i
    |> CCInt.of_string
    |> CCOption.map Pool_common.Version.of_int
    |> CCOption.to_result (Error.Invalid Field.Version)
    |> Lwt_result.lift
  in
  let* value =
    match field_id, find_param_opt Htmx.multi_select_htmx_key with
    | Some _, Some param when CCString.equal param Htmx.multi_select_htmx_value
      -> HttpUtils.htmx_urlencoded_list field_str req |> Lwt_result.ok
    | _ ->
      find_param_list field_str
      |> CCOption.to_result InvalidHtmxRequest
      |> Lwt_result.lift
  in
  (field, version, value, field_id) |> Lwt_result.return
;;

let update ?contact req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_htmx_request_boolean_values Field.[ Paused |> show ]
  in
  let result
    ({ Pool_context.database_label; language; query_parameters; user; _ } as
     context)
    =
    let is_admin = Pool_context.user_is_admin user in
    let path_with_params = HttpUtils.url_with_field_params query_parameters in
    let with_redirect path res =
      res |> CCResult.map_err (fun err -> err, path_with_params path)
    in
    let* contact =
      match contact with
      | Some contact -> Lwt_result.return contact
      | None ->
        Pool_context.find_contact context
        |> with_redirect ("/login" |> HttpUtils.intended_of_request req)
        |> Lwt_result.lift
    in
    let back_path =
      if is_admin
      then
        Format.asprintf
          "/admin/contacts/%s/edit"
          Contact.(contact |> id |> Id.value)
      else "/user/personal-details"
    in
    let* Pool_context.Tenant.{ tenant_languages; _ } =
      Pool_context.Tenant.find req |> with_redirect back_path |> Lwt_result.lift
    in
    let* field, version, value, field_id =
      parse_urlencoded
        ~is_admin
        req
        database_label
        language
        urlencoded
        Contact.(contact |> id)
      ||> with_redirect back_path
    in
    let contact_id = Contact.id contact in
    let* custom_field =
      field_id
      |> CCOption.map_or ~default:(Lwt_result.return None) (fun id ->
        Custom_field.find_by_contact ~is_admin database_label contact_id id
        ||> with_redirect back_path
        >|+ CCOption.pure)
    in
    let%lwt response =
      let open CCResult in
      let tags = Pool_context.Logger.Tags.req req in
      let html_response html =
        html |> HttpUtils.Htmx.html_to_plain_text_response |> Lwt.return
      in
      let%lwt partial_update =
        Custom_field.validate_partial_update
          ~is_admin
          contact
          custom_field
          (field, version, value)
      in
      let events =
        let open CCResult in
        partial_update
        >>= Cqrs_command.Contact_command.Update.handle ~tags user contact
      in
      let htmx_element () =
        let hx_post =
          Htmx.(
            if is_admin
            then admin_profile_hx_post contact_id
            else contact_profile_hx_post)
          |> path_with_params
          |> Sihl.Web.externalize_path
        in
        let hx_delete =
          field_id |> CCOption.map (Htmx.admin_profile_hx_delete contact_id)
        in
        match partial_update with
        | Ok partial_update ->
          let open Custom_field.PartialUpdate in
          (match partial_update with
           | Language _ when not is_admin ->
             HttpUtils.Htmx.htmx_redirect
               (Sihl.Web.externalize_path back_path)
               ()
           | Language _ | Firstname _ | Lastname _ | Custom _ ->
             Htmx.partial_update_to_htmx
               language
               tenant_languages
               is_admin
               partial_update
               ~hx_post
               ?hx_delete
               ~success:true
               ()
             |> html_response)
        | Error error ->
          let error = Pool_common.Utils.with_log_error ~src ~tags error in
          let create_htmx ?help ?htmx_attributes ?label ?(field = field) value =
            Htmx.create
              (Htmx.create_entity
                 ?help
                 ?htmx_attributes
                 ?label
                 version
                 field
                 value)
              language
              ~hx_post
              ~error
              ~required:true
              ()
            |> html_response
          in
          (match[@warning "-4"] field with
           | Field.Firstname ->
             Htmx.Text (value |> CCList.head_opt) |> create_htmx
           | Field.Lastname ->
             Htmx.Text (value |> CCList.head_opt) |> create_htmx
           | Field.Paused -> Htmx.Boolean (Some false) |> create_htmx
           | Field.Language ->
             Htmx.Select
               Htmx.
                 { show = Pool_common.Language.show
                 ; options = tenant_languages
                 ; option_formatter = None
                 ; selected =
                     CCOption.bind (value |> CCList.head_opt) (fun value ->
                       value |> Pool_common.Language.create |> CCResult.to_opt)
                 }
             |> create_htmx
                  ~label:Field.ContactLanguage
                  ~help:[ Pool_common.I18n.ContactLanguage ]
           | _ ->
             let open Custom_field in
             let%lwt field =
               let open Utils.Lwt_result.Infix in
               field_id
               |> CCOption.to_result Error.InvalidHtmxRequest
               |> Lwt_result.lift
               >>= find_by_contact ~is_admin database_label (Contact.id contact)
             in
             (match field with
              | Error error ->
                HttpUtils.(
                  Htmx.htmx_redirect
                    back_path
                    ~query_parameters
                    ~actions:[ Message.set ~error:[ error ] ]
                    ())
              | Ok field ->
                Htmx.custom_field_to_htmx
                  language
                  is_admin
                  field
                  ~version
                  ~hx_post
                  ~error
                  ~flash_values:value
                  ()
                |> html_response))
      in
      let%lwt () =
        match events with
        (* This case cannot occur, cqrs handler always returns an Ok result *)
        | Error _ -> Lwt.return_unit
        | Ok events ->
          events
          |> Lwt_list.iter_s (Pool_event.handle_event ~tags database_label user)
      in
      () |> htmx_element
    in
    response |> Lwt_result.return
  in
  HttpUtils.Htmx.extract_happy_path ~src req result
;;
