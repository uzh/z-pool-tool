module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let create_layout req = General.create_tenant_layout req

let index req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/dashboard")
    @@ let%lwt admin_users = Admin.find_all database_label () in
       Page.Admin.Admins.index context admin_users
       |> create_layout req ~active_navigation:"/admin/admins" context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let admin_detail req is_edit =
  (* TODO: Impelement authorization *)
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/admins")
    @@
    let id =
      let open Pool_common.Message.Field in
      HttpUtils.find_id Admin.Id.of_string Admin req
    in
    let* admin = id |> Admin.find database_label in
    let* () =
      let* _ = General.admin_from_session database_label req in
      Lwt.return_ok ()
    in
    (match is_edit with
     | true -> Page.Admin.Admins.edit context admin
     | false -> Page.Admin.Admins.detail context admin)
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail req = admin_detail req false
let edit req = admin_detail req true

module Access : Helpers.AccessSig = struct
  let admin_effects =
    Middleware.Guardian.id_effects Admin.Id.of_string Field.Admin
  ;;

  let create = Middleware.Guardian.denied

  let read =
    [ (fun id ->
        [ `Read, `Target (id |> Guard.Uuid.target_of Admin.Id.value)
        ; `Read, `TargetEntity (`Admin `Operator)
        ])
    ]
    |> admin_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let update =
    [ `Update, `TargetEntity (`Admin `Operator) ]
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let delete = Middleware.Guardian.denied

  let index =
    [ `Read, `TargetEntity `Filter ]
    |> Middleware.Guardian.validate_admin_entity
  ;;
end
