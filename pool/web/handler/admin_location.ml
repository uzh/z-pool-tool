module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

(* Use this to extract ids from requests, the params are not named :id, because
   two ids appear in a route *)
let id req field =
  Sihl.Web.Router.param req @@ Pool_common.Message.Field.show field
  |> Pool_common.Id.of_string
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/dashboard" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let%lwt location_list = Pool_location.find_all tenant_db in
    Page.Admin.Location.index location_list context
    |> create_layout ~active_navigation:"/locations" req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/locations" in
  let result context =
    Lwt_result.map_err (fun err -> err, error_path)
    @@ (Page.Admin.Location.form context
       |> create_layout req context
       >|= Sihl.Web.Response.of_html)
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.tenant_db; _ } =
    Lwt_result.map_err (fun err -> err, "/admin/locations/new")
    @@
    let events =
      let open CCResult.Infix in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      Cqrs_command.Location_command.Create.(
        urlencoded |> HttpUtils.remove_empty_values |> decode >>= handle)
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      Http_utils.redirect_to_with_actions
        "/admin/locations"
        [ Message.set ~success:[ Pool_common.Message.(Created Field.Location) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail edit req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/locations" in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let open Lwt_result.Syntax in
    let id =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Location
      |> Pool_location.Id.of_string
    in
    let* location = Pool_location.find tenant_db id in
    let states = Pool_location.Status.all in
    Page.Admin.Location.(
      match edit with
      | false -> detail location context
      | true -> form ~location ~states context)
    |> Lwt.return_ok
    >>= create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show = detail false
let edit = detail true

let update req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.tenant_db; _ } =
    let id =
      HttpUtils.get_field_router_param req Pool_common.Message.Field.Location
      |> Pool_location.Id.of_string
    in
    let detail_path =
      Format.asprintf "/admin/locations/%s" (id |> Pool_location.Id.value)
    in
    Lwt_result.map_err (fun err -> err, Format.asprintf "%s/edit" detail_path)
    @@
    let open Lwt_result.Syntax in
    let* location = Pool_location.find tenant_db id in
    let events =
      let open CCResult.Infix in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      Cqrs_command.Location_command.Update.(
        urlencoded
        |> HttpUtils.remove_empty_values
        |> decode
        >>= handle location)
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      Http_utils.redirect_to_with_actions
        detail_path
        [ Message.set ~success:[ Pool_common.Message.(Updated Field.Location) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
