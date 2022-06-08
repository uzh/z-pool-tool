module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

let index req =
  let open Utils.Lwt_result.Infix in
  let id =
    Pool_common.Message.Field.(Experiment |> show)
    |> Sihl.Web.Router.param req
    |> Pool_common.Id.of_string
  in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Pool_common.Id.value id)
  in
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, error_path)
    @@ let* experiment = Experiment.find tenant_db id in
       let* sessions =
         Session.find_all_for_experiment tenant_db experiment.Experiment.id
       in
       let* assignments =
         Lwt_list.map_s
           (fun session ->
             let* assignments =
               Assignment.find_by_session tenant_db session.Session.id
             in
             Lwt_result.return (session, assignments))
           sessions
         |> Lwt.map CCList.all_ok
       in
       Page.Admin.Assignment.list assignments experiment context
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;
