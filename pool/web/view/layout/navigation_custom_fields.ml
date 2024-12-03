open Entity
module NavUtils = Navigation_utils
module Field = Pool_message.Field

let read_entity entity =
  Guard.(ValidationSet.one_of_tuple (Permission.Read, entity, None))
;;

let custom_field_path model =
  Format.asprintf "/admin/custom-fields/%s" (Custom_field.Model.show model)
;;

let nav_elements =
  let open Custom_field in
  Model.(
    all
    |> CCList.map (fun model ->
      Single (custom_field_path model, to_nav_link model, AlwaysOn))
    |> CCList.map NavElement.create)
;;

let create
      ?buttons
      ?hint
      ({ Pool_context.database_label; language; user; _ } as context)
      model
      content
  =
  let open Utils.Lwt_result.Infix in
  let open Tab_navigation in
  let%lwt actor =
    Pool_context.Utils.find_authorizable database_label user
    ||> Pool_common.Utils.get_or_failwith
  in
  let title =
    Custom_field.Model.to_nav_link model
    |> Pool_common.(Utils.nav_link_to_string language)
  in
  let active_navigation = custom_field_path model in
  let html = make_body ?buttons ?hint language title content in
  let overlay_title =
    Custom_field.Model.show model |> CCString.capitalize_ascii
  in
  let title =
    Pool_common.(I18n.CustomFields |> Utils.nav_link_to_string language)
  in
  let subpage =
    make_tabs ~actor ~active_navigation ~overlay_title context html nav_elements
  in
  with_heading title subpage |> Lwt.return
;;
