open CCFun
open Utils.Lwt_result.Infix
module Field = Pool_message.Field
module HttpUtils = Http_utils

let src = Logs.Src.create "handler.helper.search"

let htmx_search_helper
  ?(query_field = Field.Search)
  ?(exclude_field = Field.Exclude)
  entity
  req
  =
  let result { Pool_context.database_label; user; language; _ } =
    let* actor =
      Pool_context.Utils.find_authorizable ~admin_only:true database_label user
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let query = HttpUtils.find_in_urlencoded_opt query_field urlencoded in
    let entities_to_exclude encode_id =
      let%lwt selected =
        HttpUtils.htmx_urlencoded_list Field.(array_key exclude_field) req
      in
      CCList.map encode_id selected |> Lwt.return
    in
    let execute_search search_fnc to_html =
      (match query with
       | None -> Lwt.return []
       | Some query -> search_fnc query actor)
      ||> to_html language
      ||> HttpUtils.Htmx.multi_html_to_plain_text_response %> CCResult.return
    in
    let open Guard.Persistence in
    match entity with
    | `Experiment ->
      let open Component.Search.Experiment in
      let open Experiment.Guard.Access in
      let%lwt exclude = entities_to_exclude Experiment.Id.of_string in
      let search_experiment value actor =
        Experiment.search ~exclude database_label value
        >|> Lwt_list.filter_s (fun (id, _) ->
          validate database_label (read id) actor ||> CCResult.is_ok)
      in
      execute_search search_experiment query_results
    | `Location ->
      let open Component.Search.Location in
      let open Pool_location.Guard.Access in
      let%lwt exclude = entities_to_exclude Pool_location.Id.of_string in
      let search_location value actor =
        Pool_location.search database_label ~exclude value
        >|> Lwt_list.filter_s (fun (id, _) ->
          validate database_label (read id) actor ||> CCResult.is_ok)
      in
      execute_search search_location query_results
    | `ContactTag ->
      let open Component.Search.Tag in
      let open Tags.Guard.Access in
      let%lwt exclude = entities_to_exclude Tags.Id.of_string in
      let search_tags value actor =
        Tags.search_by_title
          database_label
          ~model:Tags.Model.Contact
          ~exclude
          value
        >|> Lwt_list.filter_s (fun (id, _) ->
          validate database_label (read id) actor ||> CCResult.is_ok)
      in
      execute_search search_tags query_results
    | `Admin ->
      let open Component.Search.Admin in
      let open Admin.Guard.Access in
      let%lwt exclude = entities_to_exclude Pool_user.Id.of_string in
      let search_experiment value actor =
        Admin.search_by_name_and_email ~exclude database_label value
        >|> Lwt_list.filter_s (fun admin ->
          let id = Admin.id admin in
          validate database_label (read id) actor ||> CCResult.is_ok)
      in
      execute_search search_experiment query_results
  in
  result
  |> HttpUtils.Htmx.handle_error_message ~error_as_notification:true ~src req
;;
