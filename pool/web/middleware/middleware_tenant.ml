let tenant_db_of_request req
    : (Pool_common.Database.Label.t, string) result Lwt.t
  =
  (* TODO handle PREFIX_PATH of Tenant URLs, multiple tenants behind the same
     host cannot be handled at the moment *)
  let db_pool () =
    let open Lwt_result.Syntax in
    let* host =
      req
      |> Sihl.Web.Request.header "host"
      |> CCOpt.to_result "No 'host' found!"
      |> Lwt_result.lift
    in
    let* selections = Tenant.Selection.find_all () in
    CCList.assoc_opt
      ~eq:(fun m k -> CCString.prefix ~pre:m k)
      host
      (selections
      |> CCList.map (fun sel -> Tenant.Selection.(url sel, label sel)))
    |> CCOpt.to_result "No corresponding Tenant found!"
    |> Lwt_result.lift
  in
  ()
  |> db_pool
  |> Lwt_result.map_err (fun _ ->
         "Something on our side went wrong, please try again later or on multi \
          occurrences please contact the Administrator.")
;;
