module PoolField = Pool_common.Message.Field
module HttpUtils = Http_utils
(* Another type?? *)
(***

  type test = | Firstname | Lastname | Paused | Language | Custom of
  Custom_field.Id.t *)

let parse_urlencoded tenant_db language urlencoded =
  let open Pool_common.Message in
  let open Utils.Lwt_result.Syntax in
  let open Utils.Lwt_result.Infix in
  let find_param name err =
    let open CCOption in
    CCList.assoc_opt ~eq:CCString.equal name urlencoded
    >>= CCList.head_opt
    |> to_result err
    |> Lwt_result.lift
  in
  let%lwt field_id =
    find_param Htmx.field_id_key ()
    ||> CCOption.of_result
    ||> CCOption.map Custom_field.Id.of_string
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
      >>= Custom_field.find_public tenant_db
      >|= fun f -> Htmx.custom_field_label language f
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
  let* value = find_param field_str InvalidHtmxRequest in
  (field, version, value, field_id) |> Lwt_result.return
;;

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
    let* field, version, value, field_id =
      parse_urlencoded tenant_db language urlencoded
      ||> with_redirect "/user/personal-details"
    in
    let%lwt response =
      let open CCResult in
      let html_response html =
        let csrf_element =
          Htmx.csrf_element_swap csrf ~id:Htmx.user_update_csrf ()
        in
        [ html; csrf_element ]
        |> HttpUtils.multi_html_to_plain_text_response
        |> Lwt.return
      in
      let%lwt partial_update =
        Contact.validate_partial_update
          contact
          tenant_db
          (field, version, value, field_id)
      in
      let events =
        let open CCResult in
        partial_update >>= Cqrs_command.Contact_command.Update.handle contact
      in
      let htmx_element () =
        let hx_post =
          Sihl.Web.externalize_path (path_with_lang "/user/update")
        in
        let open Pool_common.Message in
        match partial_update with
        | Ok partial_update ->
          partial_update
          (* CustomFields: It updates on error?? *)
          |> Contact.PartialUpdate.increment_version
          |> fun m ->
          Htmx.partial_update_to_htmx
            language
            tenant_languages
            m
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
              ()
            |> html_response
          in
          (match[@warning "-4"] field with
           | Field.Firstname ->
             Htmx.Text (value |> CCOption.pure) |> create_htmx
           | Field.Lastname -> Htmx.Text (value |> CCOption.pure) |> create_htmx
           | Field.Paused -> Htmx.Checkbox false |> create_htmx
           | Field.Language ->
             Htmx.Select
               Htmx.
                 { show = Pool_common.Language.show
                 ; options = tenant_languages
                 ; selected =
                     value |> Pool_common.Language.create |> CCResult.to_opt
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
                    "/user/update"
                    ?query_language
                    ~actions:[ Message.set ~error:[ error ] ]
                    ())
              | Ok field ->
                let value =
                  match field with
                  | Public.Number _ -> value |> CCInt.of_string |> Htmx.number
                  | Public.Text _ -> value |> CCOption.pure |> Htmx.text
                in
                Htmx.custom_field_to_htmx
                  ~value
                  language
                  field
                  ~hx_post
                  ~error
                  ()
                |> html_response))
      in
      let%lwt () =
        match events with
        | Error _ -> Lwt.return_unit
        | Ok events ->
          events |> Lwt_list.iter_s (Pool_event.handle_event tenant_db)
      in
      () |> htmx_element
    in
    response |> Lwt_result.return
  in
  HttpUtils.extract_happy_path_htmx req result
;;
