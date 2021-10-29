type request = Rock.Request.t
type response = Rock.Response.t
type handler = request -> response Lwt.t

let urlencoded_of_request = Utils.todo
let response_of_redirect = Utils.todo
let response = Utils.todo
let handle_events _ = Utils.todo

let user_from_session req : Sihl.Contract.User.t option Lwt.t =
  let%lwt tenant_db = Middleware.Tenant.tenant_db_of_request req in
  let user tenant_db =
    Service.User.Web.user_from_session
      ~ctx:[ "pool", tenant_db |> Pool_common.Database.Label.value ]
      req
  in
  tenant_db |> CCResult.to_opt |> CCOpt.map_or ~default:Lwt.return_none user
;;
