open CCFun
open Utils.Lwt_result.Infix
module Field = Pool_common.Message.Field
module HttpUtils = Http_utils

let src = Logs.Src.create "handler.helper.search"

let create search_type ?path req =
  let query_field, path =
    match search_type with
    | `Experiment ->
      Field.Title, CCOption.value ~default:"/admin/experiments/search" path
    | `Location ->
      Field.Name, CCOption.value ~default:"/admin/locations/search" path
  in
  let result { Pool_context.database_label; user; _ } =
    let open CCList in
    let%lwt actor =
      Pool_context.Utils.find_authorizable_opt
        ~admin_only:true
        database_label
        user
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let query = HttpUtils.find_in_urlencoded_opt query_field urlencoded in
    let search_role =
      HttpUtils.find_in_urlencoded_opt Field.Role urlencoded
      |> CCOption.(flip bind (Role.Actor.of_string_res %> of_result))
    in
    let%lwt exclude_roles_of =
      HttpUtils.find_in_urlencoded_opt Field.ExcludeRolesOf urlencoded
      |> CCOption.map_or
           ~default:Lwt.return_none
           (Admin.Id.of_string
            %> fun id -> Admin.find database_label id ||> CCResult.to_opt)
      >|> CCOption.map_or
            ~default:(Lwt.return [])
            (Helpers_guard.find_roles database_label)
      ||> filter_map
            (CCOption.map_or
               ~default:(CCFun.const None)
               Role.Actor.find_target_of
               search_role)
    in
    let exclude =
      HttpUtils.find_in_urlencoded_list_opt Field.Exclude urlencoded
    in
    let open Guard.Persistence in
    match search_type with
    | `Experiment ->
      let open Component.Search.Experiment in
      let open Experiment.Guard.Access in
      let exclude = exclude >|= Experiment.Id.of_string in
      let exclude_roles_of =
        exclude_roles_of
        >|= Guard.Uuid.Target.to_string %> Experiment.Id.of_string
      in
      let search_experiment exclude value actor =
        Experiment.search database_label exclude value
        >|> Lwt_list.filter_s (fun (id, _) ->
              validate database_label (read id) actor ||> CCResult.is_ok)
      in
      (match query, actor with
       | None, _ | Some _, None -> Lwt.return []
       | Some value, Some actor ->
         search_experiment (exclude @ exclude_roles_of) value actor)
      ||> fun results ->
      input_element ?value:query ~results path
      |> HttpUtils.html_to_plain_text_response
      |> CCResult.return
    | `Location ->
      let open Component.Search.Location in
      let open Pool_location.Guard.Access in
      let exclude = exclude >|= Pool_location.Id.of_string in
      let exclude_roles_of =
        exclude_roles_of
        >|= Guard.Uuid.Target.to_string %> Pool_location.Id.of_string
      in
      let search_location exclude value actor =
        Pool_location.search database_label exclude value
        >|> Lwt_list.filter_s (fun (id, _) ->
              validate database_label (read id) actor ||> CCResult.is_ok)
      in
      (match query, actor with
       | None, _ | Some _, None ->
         Tyxml.Html.txt ""
         |> HttpUtils.html_to_plain_text_response
         |> Lwt_result.return
       | Some value, Some actor ->
         let%lwt results =
           search_location (exclude @ exclude_roles_of) value actor
         in
         input_element ?value:query ~results path
         |> HttpUtils.html_to_plain_text_response
         |> Lwt_result.return)
  in
  result |> HttpUtils.htmx_handle_error_message ~src req
;;
