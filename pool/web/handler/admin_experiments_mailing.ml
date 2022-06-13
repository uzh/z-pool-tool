module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let create_layout req = General.create_tenant_layout `Admin req

let id req field encode =
  Sihl.Web.Router.param req @@ Field.show field |> encode
;;

let experiment_path ?(suffix = "") experiment_id =
  Format.asprintf
    "/admin/experiments/%s/%s"
    (Pool_common.Id.value experiment_id)
    suffix
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Field.Experiment Pool_common.Id.of_string in
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, experiment_path experiment_id)
    @@ let* experiment = Experiment.find tenant_db experiment_id in
       let%lwt mailings =
         Mailing.find_by_experiment tenant_db experiment.Experiment.id
       in
       Page.Admin.Mailing.index context experiment mailings
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Field.Experiment Pool_common.Id.of_string in
  let result context =
    Lwt_result.map_err (fun err -> err, experiment_path experiment_id)
    @@ (Page.Admin.Mailing.form
          context
          experiment_id
          (CCFun.flip Sihl.Web.Flash.find req)
       |> create_layout req context
       >|= Sihl.Web.Response.of_html)
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Field.Experiment Pool_common.Id.of_string in
  let redirect_path = experiment_path ~suffix:"mailings" experiment_id in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@ let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
       let* experiment = Experiment.find tenant_db experiment_id in
       let events =
         let open CCResult in
         let open Cqrs_command.Mailing_command.Create in
         urlencoded |> decode >>= handle experiment
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set
               ~success:[ Pool_common.Message.(Created Field.Mailing) ]
           ]
       in
       events |> Lwt_result.lift |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail edit req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Field.Experiment Pool_common.Id.of_string in
  let id = id req Field.Mailing Mailing.Id.of_string in
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err ->
        err, experiment_path ~suffix:"mailings" experiment_id)
    @@ let* mailing = Mailing.find tenant_db id in
       (match edit with
       | false -> Page.Admin.Mailing.detail context experiment_id mailing
       | true ->
         Page.Admin.Mailing.form
           ~mailing
           context
           experiment_id
           (CCFun.flip Sihl.Web.Flash.find req))
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show = detail false
let edit = detail true
