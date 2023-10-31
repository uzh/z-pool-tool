open CCFun
open Utils.Lwt_result.Infix
module Field = Pool_common.Message.Field
module HttpUtils = Http_utils

let src = Logs.Src.create "handler.helper.search"

let create search_type req =
  let query_field = Field.Search in
  let result { Pool_context.database_label; user; language; _ } =
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
      let open CCOption in
      HttpUtils.find_in_urlencoded_opt Field.Role urlencoded
      |> flip bind (fun role ->
        try Role.Role.of_string role |> return with
        | _ -> None)
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
      ||> filter_map (fun ({ Guard.ActorRole.target_uuid; role; _ }, _, _) ->
        CCOption.bind search_role (fun search_role ->
          if Role.Role.equal role search_role then None else target_uuid))
    in
    let exclude =
      HttpUtils.find_in_urlencoded_list_opt Field.Exclude urlencoded
    in
    let to_response html =
      html
      |> HttpUtils.Htmx.multi_html_to_plain_text_response
      |> CCResult.return
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
      ||> query_results language
      ||> to_response
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
         |> HttpUtils.Htmx.html_to_plain_text_response
         |> Lwt_result.return
       | Some value, Some actor ->
         search_location (exclude @ exclude_roles_of) value actor
         ||> query_results language
         ||> to_response)
    | `ContactTag ->
      let open Component.Search.Tag in
      let open Tags.Guard.Access in
      let exclude = exclude >|= Tags.Id.of_string in
      let exclude_roles_of =
        exclude_roles_of >|= Guard.Uuid.Target.to_string %> Tags.Id.of_string
      in
      let search_tags exclude value actor =
        Tags.search_by_title
          database_label
          ~model:Tags.Model.Contact
          ~exclude
          value
        >|> Lwt_list.filter_s (fun (id, _) ->
          validate database_label (read id) actor ||> CCResult.is_ok)
      in
      (match query, actor with
       | None, _ | Some _, None -> Lwt.return []
       | Some value, Some actor ->
         search_tags (exclude @ exclude_roles_of) value actor)
      ||> query_results language
      ||> to_response
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;
