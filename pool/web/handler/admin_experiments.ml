module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Invitations = Admin_experiments_invitations
module WaitingList = Admin_experiments_waiting_list
module Assignment = Admin_experiments_assignments
module Mailings = Admin_experiments_mailing

let create_layout req = General.create_tenant_layout req

let id req field encode =
  Sihl.Web.Router.param req @@ Pool_common.Message.Field.show field |> encode
;;

let experiment_boolean_fields =
  Experiment.boolean_fields |> CCList.map Pool_common.Message.Field.show
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let error_path = "/admin/dashboard" in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, error_path)
    @@ let%lwt expermient_list = Experiment.find_all tenant_db () in
       Page.Admin.Experiments.index expermient_list context
       |> create_layout ~active_navigation:"/admin/experiments" req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let open Lwt_result.Syntax in
  let error_path = "/admin/experiments" in
  let result context =
    Lwt_result.map_error (fun err -> err, error_path)
    @@
    let flash_fetcher key = Sihl.Web.Flash.find key req in
    let* sys_languages =
      Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
    in
    Page.Admin.Experiments.create context sys_languages flash_fetcher
    |> create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let%lwt urlencoded =
    Sihl.Web.Request.to_urlencoded req
    ||> HttpUtils.format_request_boolean_values experiment_boolean_fields
    ||> HttpUtils.remove_empty_values
  in
  let result { Pool_context.tenant_db; _ } =
    Lwt_result.map_error (fun err ->
      ( err
      , "/admin/experiments/create"
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let events =
      let open CCResult.Infix in
      urlencoded
      |> Cqrs_command.Experiment_command.Create.decode
      >>= Cqrs_command.Experiment_command.Create.handle
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      Http_utils.redirect_to_with_actions
        "/admin/experiments"
        [ Message.set
            ~success:[ Pool_common.Message.(Created Field.Experiment) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let detail edit req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, "/admin/experiments")
    @@
    let open Lwt_result.Syntax in
    let id = Pool_common.(id req Message.Field.Experiment Id.of_string) in
    let* experiment = Experiment.find tenant_db id in
    (match edit with
     | false ->
       let* session_count = Experiment.session_count tenant_db id in
       Page.Admin.Experiments.detail experiment session_count context
       |> Lwt.return_ok
     | true ->
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       let* sys_languages =
         Pool_context.Tenant.get_tenant_languages req |> Lwt_result.lift
       in
       Page.Admin.Experiments.edit
         experiment
         context
         sys_languages
         flash_fetcher
       |> Lwt.return_ok)
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
    let id = Pool_common.(id req Message.Field.Experiment Id.of_string) in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.format_request_boolean_values experiment_boolean_fields
      ||> HttpUtils.remove_empty_values
    in
    let detail_path =
      Format.asprintf "/admin/experiments/%s" (id |> Pool_common.Id.value)
    in
    Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf "%s/edit" detail_path
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let open Lwt_result.Syntax in
    let* experiment = Experiment.find tenant_db id in
    let events =
      let open CCResult.Infix in
      let open Cqrs_command.Experiment_command.Update in
      urlencoded |> decode >>= handle experiment |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      Http_utils.redirect_to_with_actions
        detail_path
        [ Message.set
            ~success:[ Pool_common.Message.(Updated Field.Experiment) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let delete req =
  let open Utils.Lwt_result.Infix in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    let experiment_id =
      Pool_common.(id req Pool_common.Message.Field.Experiment Id.of_string)
    in
    let experiments_path = "/admin/experiments" in
    Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf
          "%s/%s"
          experiments_path
          (Pool_common.Id.value experiment_id) ))
    @@ let* session_count = Experiment.session_count tenant_db experiment_id in
       let events =
         Cqrs_command.Experiment_command.Delete.(
           handle { experiment_id; session_count })
         |> Lwt_result.lift
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           experiments_path
           [ Message.set
               ~success:[ Pool_common.Message.(Created Field.Experiment) ]
           ]
       in
       events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
