open Utils.Lwt_result.Infix
module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_message.Field

let src = Logs.Src.create "handler.admin.location"
let create_layout req = General.create_tenant_layout req
let id req field encode = Sihl.Web.Router.param req @@ Field.show field |> encode

let descriptions_from_urlencoded req urlencoded =
  let open Pool_location in
  let open CCOption in
  let tenant_languages = Pool_context.Tenant.get_tenant_languages_exn req in
  tenant_languages
  |> CCList.filter_map (fun language ->
    CCList.assoc_opt ~eq:CCString.equal (Description.field_name language) urlencoded
    >>= CCList.head_opt
    >|= CCPair.make language)
  |> function
  | [] -> Ok None
  | lst -> Description.create tenant_languages lst |> CCResult.map return
;;

let index req =
  HttpUtils.Htmx.handler
    ~active_navigation:"/admin/locations"
    ~error_path:"/admin/locations"
    ~query:(module Pool_location)
    ~create_layout
    req
  @@ fun ({ Pool_context.database_label; user; _ } as context) query ->
  let* actor =
    Pool_context.Utils.find_authorizable ~admin_only:true database_label user
  in
  let%lwt location_list, query = Pool_location.list_by_user ~query database_label actor in
  let open Page.Admin.Location in
  (if HttpUtils.Htmx.is_hx_request req then list else index) context location_list query
  |> Lwt_result.return
;;

let new_form req =
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/locations")
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let tenant_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    Page.Admin.Location.form context tenant_languages flash_fetcher
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let create req =
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values [ Field.(Virtual |> show) ]
    ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, "/admin/locations/create", [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* description = descriptions_from_urlencoded req urlencoded |> Lwt_result.lift in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Location_command.Create in
      urlencoded
      |> HttpUtils.format_request_boolean_values [ Field.(Virtual |> show) ]
      |> HttpUtils.remove_empty_values
      |> decode description
      >>= handle ~tags
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        "/admin/locations"
        [ Message.set ~success:[ Pool_message.(Success.Created Field.Location) ] ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let new_file req =
  let open Pool_location in
  let id = id req Field.Location Id.of_string in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err ->
      err, id |> Id.value |> Format.asprintf "/admin/locations/%s/files/create")
    @@ let* location = find database_label id in
       let labels = File.Label.all in
       let languages = Pool_common.Language.all in
       Page.Admin.Location.file_form labels languages location context
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let add_file req =
  let id =
    HttpUtils.get_field_router_param req Field.Location |> Pool_location.Id.of_string
  in
  let path = id |> Pool_location.Id.value |> Format.asprintf "/admin/locations/%s" in
  let result { Pool_context.database_label; user; _ } =
    Utils.Lwt_result.map_error (fun err -> err, Format.asprintf "%s/files/create" path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* location = Pool_location.find database_label id in
    let%lwt multipart_encoded = Sihl.Web.Request.to_multipart_form_data_exn req in
    let* files =
      HttpUtils.File.upload_files database_label [ Field.(FileMapping |> show) ] req
    in
    let finalize = function
      | Ok resp -> Lwt.return_ok resp
      | Error err ->
        let%lwt () =
          Lwt_list.iter_s
            (fun (_, asset_id) -> asset_id |> Storage.delete database_label)
            files
        in
        Logs.err (fun m -> m ~tags "One of the events failed while adding a file");
        Lwt.return_error err
    in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Location_command.AddFile in
      files @ multipart_encoded
      |> HttpUtils.File.multipart_form_data_to_urlencoded
      |> decode
      >>= handle ~tags location
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Pool_event.handle_events ~tags database_label user events in
      Http_utils.redirect_to_with_actions
        path
        [ Message.set ~success:[ Pool_message.(Success.Created Field.FileMapping) ] ]
    in
    events >|> finalize |>> handle
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let asset = Contact_location.asset

let detail edit req =
  let open Pool_location in
  let error_path = "/admin/locations" in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let id = id req Field.Location Id.of_string in
    let* location = find database_label id in
    let tenant_languages = Pool_context.Tenant.get_tenant_languages_exn req in
    let states = Status.all in
    Page.Admin.Location.(
      match edit with
      | false ->
        let%lwt statistics = Statistics.create database_label id in
        let%lwt statistics_year_range = Statistics.year_select database_label in
        let%lwt files = Pool_location.files_by_location database_label id in
        detail location files statistics statistics_year_range context |> Lwt.return
      | true ->
        let flash_fetcher key = Sihl.Web.Flash.find key req in
        form ~location ~states context tenant_languages flash_fetcher |> Lwt.return)
    |> Lwt_result.ok
    >>= create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let changelog req =
  let open Pool_location in
  let id = id req Field.Location Id.of_string in
  let url = HttpUtils.Url.Admin.location_path ~suffix:"changelog" ~id () in
  Helpers.Changelog.htmx_handler ~url (Id.to_common id) req
;;

let show = detail false
let edit = detail true

let statistics req =
  let open Pool_location in
  let id = id req Field.Location Id.of_string in
  let result { Pool_context.database_label; language; _ } =
    let* year =
      HttpUtils.find_query_param
        req
        Field.Year
        CCFun.Infix.(
          CCInt.of_string %> CCOption.to_result Pool_message.(Error.Invalid Field.Year))
      |> Lwt_result.lift
    in
    let%lwt statistics = Statistics.create ~year database_label id in
    let%lwt statistics_year_range = Statistics.year_select database_label in
    Page.Admin.Location.make_statistics ~year statistics_year_range language id statistics
    |> Http_utils.Htmx.html_to_plain_text_response
    |> Lwt.return_ok
  in
  result |> Http_utils.Htmx.handle_error_message ~error_as_notification:true ~src req
;;

let update req =
  let result { Pool_context.database_label; user; _ } =
    let id = id req Field.Location Pool_location.Id.of_string in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.format_request_boolean_values [ Field.(Virtual |> show) ]
      ||> HttpUtils.remove_empty_values
    in
    let detail_path =
      id |> Pool_location.Id.value |> Format.asprintf "/admin/locations/%s"
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/edit" detail_path
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@ let* location = Pool_location.find database_label id in
       let* description =
         descriptions_from_urlencoded req urlencoded |> Lwt_result.lift
       in
       let tags = Pool_context.Logger.Tags.req req in
       let events =
         let open CCResult.Infix in
         let open Cqrs_command.Location_command.Update in
         urlencoded |> decode description >>= handle ~tags location |> Lwt_result.lift
       in
       let handle events =
         let%lwt () = Pool_event.handle_events ~tags database_label user events in
         Http_utils.redirect_to_with_actions
           detail_path
           [ Message.set ~success:[ Pool_message.(Success.Updated Field.Location) ] ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let delete req =
  let result { Pool_context.database_label; user; _ } =
    let location_id = id req Field.Location Pool_location.Id.of_string in
    let file_id = id req Field.FileMapping Pool_location.File.Id.of_string in
    let path =
      location_id |> Pool_location.Id.value |> Format.asprintf "/admin/locations/%s"
    in
    Utils.Lwt_result.map_error (fun err -> err, path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* events =
      file_id |> Cqrs_command.Location_command.DeleteFile.handle ~tags |> Lwt_result.lift
    in
    let%lwt () = Pool_event.handle_events ~tags database_label user events in
    Http_utils.redirect_to_with_actions
      path
      [ Message.set ~success:[ Pool_message.(Success.Deleted Field.File) ] ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let session req =
  let location_id = id req Field.Location Pool_location.Id.of_string in
  let error_path = HttpUtils.Url.Admin.location_path ~id:location_id () in
  HttpUtils.Htmx.handler ~error_path ~create_layout ~query:(module Assignment) req
  @@ fun ({ Pool_context.database_label; _ } as context) query ->
  let* location = Pool_location.find database_label location_id in
  let session_id = id req Field.Session Session.Id.of_string in
  let* session = Session.find database_label session_id in
  let%lwt assignments =
    Assignment.find_for_session_detail_screen ~query database_label session_id
  in
  Page.Admin.Location.(
    if HttpUtils.Htmx.is_hx_request req then assignment_list else session)
    context
    location
    session
    assignments
  |> Lwt_result.return
;;

let search = Helpers.Search.htmx_search_helper `Location

module Access : sig
  include module type of Helpers.Access

  val create_file : Rock.Middleware.t
  val read_file : Rock.Middleware.t
  val delete_file : Rock.Middleware.t
  val search : Rock.Middleware.t
  val read_session : Rock.Middleware.t
end = struct
  include Helpers.Access
  module LocationCommand = Cqrs_command.Location_command
  module Guardian = Middleware.Guardian

  let file_effects = Guardian.id_effects Pool_location.File.Id.validate Field.File
  let location_effects = Guardian.id_effects Pool_location.Id.validate Field.Location

  let combined_file_effects validation_set =
    let open CCResult.Infix in
    Guardian.validate_generic
    @@ fun req ->
    let* location_id = id req Field.Location Pool_location.Id.validate in
    let* file_id = id req Field.File Pool_location.File.Id.validate in
    validation_set location_id file_id |> CCResult.return
  ;;

  let combined_ocation_effects validation_set =
    let open CCResult.Infix in
    Guardian.validate_generic
    @@ fun req ->
    let* location_id = id req Field.Location Pool_location.Id.validate in
    let* session_id = id req Field.Session Session.Id.validate in
    validation_set location_id session_id |> CCResult.return
  ;;

  let index =
    Pool_location.Guard.Access.index |> Guardian.validate_admin_entity ~any_id:true
  ;;

  let create = LocationCommand.Create.effects |> Guardian.validate_admin_entity
  let create_file = location_effects LocationCommand.AddFile.effects
  let read = location_effects Pool_location.Guard.Access.read
  let read_file = file_effects Pool_location.Guard.Access.File.read
  let update = location_effects LocationCommand.Update.effects
  let delete_file = combined_file_effects LocationCommand.DeleteFile.effects
  let search = index
  let read_session = combined_ocation_effects Session.Guard.Access.read_by_location
end
