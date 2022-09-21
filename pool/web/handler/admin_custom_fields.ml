module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

let boolean_fields =
  Custom_field.boolean_fields |> CCList.map Pool_common.Message.Field.show
;;

let find_assocs_in_urlencoded urlencoded field =
  let field = Pool_common.Message.Field.array_key field in
  CCList.filter_map
    (fun (key, values) ->
      let key, lang = CCString.take_drop (CCString.length field) key in
      match CCString.equal field key with
      | false -> None
      | true ->
        let language =
          let open CCOption in
          lang
          |> CCString.chop_prefix ~pre:"["
          >>= CCString.chop_suffix ~suf:"]"
          >>= fun l -> l |> Pool_common.Language.create |> CCResult.to_opt
        in
        let value = CCList.head_opt values in
        (match language, value with
         | Some l, Some v -> Some (l, v)
         | _ -> None))
    urlencoded
;;

let id req field encode =
  Sihl.Web.Router.param req @@ Pool_common.Message.Field.show field |> encode
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@ let%lwt field_list = Custom_field.find_all tenant_db () in
       Page.Admin.CustomFields.index field_list context
       |> create_layout ~active_navigation:"/admin/custom-fields" req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let form ?id req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/admin/custom-fields")
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* custom_field =
      match id with
      | Some id ->
        let* field = Custom_field.find tenant_db id in
        Lwt_result.return (Some field)
      | None -> Lwt_result.return None
    in
    let%lwt sys_languages = Settings.find_languages tenant_db in
    Page.Admin.CustomFields.detail
      ?custom_field
      context
      sys_languages
      flash_fetcher
    |> create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req = form req

let edit req =
  let id =
    id req Pool_common.Message.Field.CustomField Custom_field.Id.of_string
  in
  form ~id req
;;

let write ?id req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values boolean_fields
    ||> HttpUtils.remove_empty_values
  in
  let field_names =
    find_assocs_in_urlencoded urlencoded Pool_common.Message.Field.Name
  in
  let field_hints =
    find_assocs_in_urlencoded urlencoded Pool_common.Message.Field.Hint
  in
  let redirect_path = "/admin/custom-fields" in
  let error_path =
    match id with
    | None -> Format.asprintf "%s/new" redirect_path
    | Some id ->
      Format.asprintf "%s/%s/edit" redirect_path (Custom_field.Id.value id)
  in
  let result { Pool_context.tenant_db; _ } =
    Lwt_result.map_error (fun err ->
      err, error_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let events =
      let open Lwt_result.Syntax in
      let%lwt sys_languages = Settings.find_languages tenant_db in
      let* decoded =
        urlencoded
        |> Cqrs_command.Custom_field_command.base_decode
        |> Lwt_result.lift
      in
      match id with
      | None ->
        Cqrs_command.Custom_field_command.Create.handle
          sys_languages
          field_names
          field_hints
          decoded
        |> Lwt_result.lift
      | Some id ->
        let* custom_field = Custom_field.find tenant_db id in
        Cqrs_command.Custom_field_command.Update.handle
          sys_languages
          custom_field
          field_names
          field_hints
          decoded
        |> Lwt_result.lift
    in
    let handle events =
      let%lwt (_ : unit list) =
        Lwt_list.map_s (Pool_event.handle_event tenant_db) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set
            ~success:[ Pool_common.Message.(Created Field.CustomField) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let create = write

let update req =
  let id =
    id req Pool_common.Message.Field.CustomField Custom_field.Id.of_string
  in
  write ~id req
;;
