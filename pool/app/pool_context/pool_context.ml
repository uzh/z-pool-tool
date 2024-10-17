include Entity

let is_from_root { database_label; _ } = Database.is_root database_label

let user_is_admin = function
  | Guest | Contact _ -> false
  | Admin _ -> true
;;

let get_admin_user = function
  | Guest | Contact _ -> Error Pool_message.(Error.NotFound Field.Admin)
  | Admin admin -> Ok admin
;;

module Utils = struct
  let find_authorizable_opt ?(admin_only = false) database_label user =
    let open Utils.Lwt_result.Infix in
    match user with
    | Contact _ when Database.is_root database_label -> Lwt.return_none
    | Contact contact when not admin_only ->
      Contact.id contact
      |> Guard.Uuid.actor_of Contact.Id.value
      |> Guard.Persistence.Actor.find database_label
      ||> CCOption.of_result
    | Guest | Contact _ -> Lwt.return_none
    | Admin admin ->
      Admin.id admin
      |> Guard.Uuid.actor_of Admin.Id.value
      |> Guard.Persistence.Actor.find database_label
      ||> CCOption.of_result
  ;;

  let find_authorizable ?admin_only database_label =
    let open CCFun in
    let open Pool_message in
    let field =
      if CCOption.value ~default:false admin_only
      then Field.Admin
      else Field.User
    in
    find_authorizable_opt ?admin_only database_label
    %> Lwt.map (CCOption.to_result (Error.NotFound field))
  ;;

  let find_query_param params field =
    CCList.assoc_opt ~eq:Pool_message.Field.equal field params
  ;;

  let remove_query_param params field =
    CCList.remove_assoc ~eq:Pool_message.Field.equal field params
  ;;

  let query_language tenant_languages query_parameters =
    let open Pool_message in
    let open CCOption.Infix in
    CCList.assoc_opt ~eq:Field.equal Field.Language query_parameters
    >>= Pool_common.Language.read_opt
    >>= fun lang ->
    CCList.find_opt (Pool_common.Language.equal lang) tenant_languages
  ;;

  let url_parameters_by_user req =
    let open Pool_message in
    let filter_params allow_list =
      allow_list
      |> CCList.filter_map (fun field ->
        Opium.Request.query Field.(human_url field) req
        |> CCOption.map (CCPair.make field))
    in
    function
    | Admin _ -> []
    | Contact _ -> filter_params Field.[ Redirected; Language ]
    | Guest ->
      filter_params Field.[ Redirected; Language; Location; SignUpCode ]
  ;;
end

module Logger = struct
  module Tags = struct
    let req (req : Sihl.Web.Request.t) : Logs.Tag.set =
      let open CCOption in
      let default = "undefined" in
      let id = Sihl.Web.Id.find req |> value ~default in
      let ip = Opium.Request.header "X-Real-IP" req |> value ~default in
      let database_label, user =
        find req
        |> of_result
        >|= (fun { database_label; user; _ } ->
              database_label |> Database.Label.value, user |> show_log_user)
        |> value ~default:(default, default)
      in
      let open Logs.Tag in
      empty
      |> add Logger.tag_req id
      |> add Logger.tag_database database_label
      |> add Logger.tag_user user
      |> add Logger.tag_ip ip
    ;;

    let context { database_label; user; _ } : Logs.Tag.set =
      let open Logs.Tag in
      empty
      |> add Logger.tag_database (database_label |> Database.Label.value)
      |> add Logger.tag_user (user |> show_log_user)
    ;;
  end
end
