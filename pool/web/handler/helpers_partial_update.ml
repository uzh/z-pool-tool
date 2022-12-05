module PoolField = Pool_common.Message.Field
module HttpUtils = Http_utils

let parse_urlencoded req tenant_db language urlencoded contact_id =
  let open Pool_common.Message in
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
      try Ok (PoolField.read field_str) with
      | _ -> Error InvalidHtmxRequest
    in
    match parsed with
    | Ok field -> field |> Lwt.return_ok
    | Error _ ->
      field_id
      |> CCOption.to_result InvalidHtmxRequest
      |> Lwt_result.lift
      >>= Custom_field.find_by_contact tenant_db contact_id
      >|+ fun f -> Custom_field.Public.to_common_field language f
  in
  let* version =
    find_param "version" (HtmxVersionNotFound field_str)
    >>= fun i ->
    i
    |> CCInt.of_string
    |> CCOption.map Pool_common.Version.of_int
    |> CCOption.to_result Pool_common.Message.(Invalid Field.Version)
    |> Lwt_result.lift
  in
  let* value =
    match field_id, find_param_opt Htmx.multi_select_key with
    | Some _, Some param when CCString.equal param Htmx.multi_select_value ->
      HttpUtils.htmx_urlencoded_list field_str req |> Lwt_result.ok
    | _ ->
      find_param_list field_str
      |> CCOption.to_result InvalidHtmxRequest
      |> Lwt_result.lift
  in
  (field, version, value, field_id) |> Lwt_result.return
;;

let update ?contact req =
  let is_admin = CCOption.is_some contact in
  let open Utils.Lwt_result.Infix in
  let open Pool_common.Message in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_htmx_request_boolean_values Field.[ Paused |> show ]
  in
  let result
    ({ Pool_context.tenant_db; language; query_language; _ } as context)
    =
    let path_with_lang = HttpUtils.path_with_language query_language in
    let with_redirect path res =
      res |> CCResult.map_err (fun err -> err, path_with_lang path)
    in
    let flash_fetcher key =
      CCList.assoc_opt ~eq:CCString.equal key urlencoded
      |> CCFun.flip CCOption.bind CCList.head_opt
    in
    let* contact =
      match contact with
      | Some contact -> Lwt_result.return contact
      | None ->
        Pool_context.find_contact context
        |> with_redirect "/login"
        |> Lwt_result.lift
    in
    let back_path =
      if is_admin
      then
        Format.asprintf
          "/admin/contacts/%s/edit"
          (contact |> Contact.id |> Pool_common.Id.value)
      else "/user/personal-details"
    in
    let* Pool_context.Tenant.{ tenant_languages; _ } =
      Pool_context.Tenant.find req |> with_redirect back_path |> Lwt_result.lift
    in
    let* field, version, value, field_id =
      parse_urlencoded req tenant_db language urlencoded Contact.(contact |> id)
      ||> with_redirect back_path
    in
    let%lwt response =
      let open CCResult in
      let html_response html =
        [ html ] |> HttpUtils.multi_html_to_plain_text_response |> Lwt.return
      in
      let%lwt partial_update =
        Contact.validate_partial_update
          ~is_admin
          contact
          tenant_db
          (field, version, value, field_id)
      in
      let tags = Logger.req req in
      let events =
        let open CCResult in
        partial_update
        >>= Cqrs_command.Contact_command.Update.handle ~tags contact
      in
      let htmx_element () =
        let hx_post =
          Htmx.(
            if is_admin
            then admin_profile_hx_post (Contact.id contact)
            else contact_profile_hx_post)
          |> path_with_lang
          |> Sihl.Web.externalize_path
        in
        let open Pool_common.Message in
        match partial_update with
        | Ok partial_update ->
          Htmx.partial_update_to_htmx
            language
            tenant_languages
            is_admin
            partial_update
            ~hx_post
            ~success:true
            ()
          |> html_response
        | Error error ->
          let create_htmx ?htmx_attributes ?(field = field) value =
            Htmx.create
              (Htmx.create_entity ?htmx_attributes version field value)
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
           | Field.Paused -> Htmx.Boolean false |> create_htmx
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
           | _ ->
             let open Custom_field in
             let%lwt field =
               let open Utils.Lwt_result.Infix in
               field_id
               |> CCOption.to_result InvalidHtmxRequest
               |> Lwt_result.lift
               >>= find_by_contact tenant_db (Contact.id contact)
             in
             (match field with
              | Error error ->
                HttpUtils.(
                  htmx_redirect
                    back_path
                    ?query_language
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
                  ~flash_fetcher
                  ()
                |> html_response))
      in
      let%lwt () =
        match events with
        (* This case cannot occur, cqrs handler always returns an Ok result *)
        | Error _ -> Lwt.return_unit
        | Ok events ->
          events |> Lwt_list.iter_s (Pool_event.handle_event ~tags tenant_db)
      in
      () |> htmx_element
    in
    response |> Lwt_result.return
  in
  HttpUtils.extract_happy_path_htmx req result
;;
