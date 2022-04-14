let tenant_of_request req =
  (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same
     host cannot be handled at the moment *)
  let open Lwt_result.Syntax in
  let* host =
    req
    |> Sihl.Web.Request.header "host"
    |> CCOption.to_result Pool_common.Message.(NotFound Host)
    |> Lwt_result.lift
  in
  let%lwt selection = Pool_tenant.find_by_url_prefix host in
  selection |> Lwt.return
;;

let tenant_db_of_request req
    : (Pool_database.Label.t, Pool_common.Message.error) result Lwt.t
  =
  let open Utils.Lwt_result in
  tenant_of_request req >|= fun t -> t.Pool_tenant.database_label
;;
