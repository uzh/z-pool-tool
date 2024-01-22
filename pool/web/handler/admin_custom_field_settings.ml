module HttpUtils = Http_utils
module Message = Pool_common.Message
module Url = Page.Admin.CustomFields.Url

let src = Logs.Src.create "handler.admin.custom_field_settings"
let create_layout req = General.create_tenant_layout req

let boolean_fields =
  Custom_field.boolean_fields |> CCList.map Message.Field.show
;;

(* let model_from_router req = let open Custom_field in let open CCResult in
   HttpUtils.find_field_router_param_opt req Message.Field.Model |>
   CCOption.to_result Message.(NotFound Field.Model) >>= Model.create ;;

   let get_model fnc req = let model = model_from_router req in match model with
   | Ok model -> model |> fnc req | Error err ->
   Http_utils.redirect_to_with_actions Url.fallback_path [ HttpUtils.Message.set
   ~error:[ err ] ] ;; *)

let settings_path = "/admin/custom-fields/settings"

let show req =
  let open Utils.Lwt_result.Infix in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, "/admin/custom-fields")
    @@
    let%lwt contact_fields =
      Custom_field.(find_by_model database_label Model.Contact)
    in
    Page.Admin.CustomFieldSettings.show context contact_fields
    |> create_layout ~active_navigation:settings_path req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

(* let post req = let open Utils.Lwt_result.Infix in let result ({
   Pool_context.database_label; _ } as context) = Utils.Lwt_result.map_error
   (fun err -> err, "/admin/custom-fields") @@ let%lwt contact_fields =
   Custom_field.(find_by_model database_label Model.Contact) in
   Page.Admin.CustomFieldSettings.show context contact_fields |> create_layout
   ~active_navigation:settings_path req context >|+ Sihl.Web.Response.of_html in
   result |> HttpUtils.extract_happy_path ~src req ;; *)

module Access : sig
  val show : Rock.Middleware.t
end = struct
  (* module CustomFieldCommand = Cqrs_command.Custom_field_command *)
  module Guardian = Middleware.Guardian

  (* let custom_field_effects = Guardian.id_effects Custom_field.Id.of_string
     Field.CustomField ;; *)

  let show =
    Custom_field.Guard.Access.index
    |> Guardian.validate_admin_entity ~any_id:true
  ;;

  (* let update = CustomFieldCommand.Update.effects |> custom_field_effects |>
     Guardian.validate_generic ;; *)
end
