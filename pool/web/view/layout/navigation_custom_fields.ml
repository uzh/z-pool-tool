open Entity
open Tyxml.Html
module NavUtils = Navigation_utils
module Field = Pool_common.Message.Field

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

let with_heading language children =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ Pool_common.(
            I18n.CustomFields |> Utils.nav_link_to_string language |> txt)
        ]
    ; children
    ]
;;

let create
  ?buttons
  ?hint
  ({ Pool_context.database_label; language; user; _ } as context)
  model
  content
  =
  let open Utils.Lwt_result.Infix in
  let open Component.Navigation in
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
  let%lwt subpage =
    NavUtils.create_main
      ~actor
      ~active_navigation
      ~validate:true
      context
      nav_elements
      false
    ||> make_tabs html
  in
  with_heading language subpage |> Lwt.return
;;
