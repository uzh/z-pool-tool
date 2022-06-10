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
       let%lwt mailings =
         Mailing.find_by_experiment tenant_db experiment.Experiment.id
       in
       Page.Admin.Mailing.index context experiment mailings
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;
