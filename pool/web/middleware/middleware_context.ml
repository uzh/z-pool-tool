open CCFun

let database_label_of_request is_root req =
  let open Pool_context in
  let tenant_database_label_of_request req =
    match Tenant.find req with
    | Ok { Tenant.tenant; _ } -> Ok tenant.Pool_tenant.database_label
    | Error _ -> Error Pool_message.(Error.Missing Field.Context)
  in
  if is_root then Ok Database.Pool.Root.label else tenant_database_label_of_request req
;;

let context_notification database_label user is_root =
  let open Utils.Lwt_result.Infix in
  let open Pool_context in
  let open Notitification in
  let%lwt root_announcement =
    match is_root with
    | true -> Lwt.return_none
    | false ->
      let context =
        match user with
        | Admin admin -> Some (`Admin, Admin.(admin |> id |> Id.to_common))
        | Contact contact -> Some (`Contact, Contact.(contact |> id |> Id.to_common))
        | Guest -> None
      in
      context
      |> CCOption.map_or
           ~default:Lwt.return_none
           (Announcement.find_by_user database_label)
      ||> CCOption.map (fun a -> Root a)
  in
  let%lwt tenant_announcements =
    let open Pool_common in
    let smtp () =
      let hint =
        { hint = I18n.SmtpMissing
        ; style = `Error
        ; link = Some (Http_utils.Url.Admin.smtp_settings_path (), I18n.Smtp)
        }
      in
      Email.SmtpAuth.defalut_is_set database_label
      >|> function
      | true -> Lwt.return_none
      | false -> Lwt.return_some (Tenant hint)
    in
    let contact_paused () =
      Lwt.return
      @@
      match user with
      | Contact contact when Contact.is_paused contact ->
        let hint =
          { hint = I18n.ContactAccountPaused
          ; style = `Warning
          ; link =
              Some
                ( Http_utils.Url.Contact.profile_path "personal-details"
                , I18n.PersonalDetails )
          }
        in
        Some (Tenant hint)
      | Contact _ | Admin _ | Guest -> None
    in
    Lwt_list.map_s (fun f -> f ())
    @@
    match user with
    | Admin _ -> [ smtp ]
    | Contact _ -> [ contact_paused ]
    | Guest -> []
  in
  root_announcement :: tenant_announcements |> CCList.filter_map CCFun.id |> Lwt.return
;;

let context () =
  let open Utils.Lwt_result.Infix in
  let open Pool_context in
  let find_query_language parameters =
    let open Pool_message.Field in
    let open CCOption.Infix in
    CCList.assoc_opt ~eq:equal Language parameters >>= Pool_common.Language.read_opt
  in
  let contact_language = function
    | None -> None
    | Some (p : Contact.t) -> p.Contact.language
  in
  let request_language url_parameters tenant_db contact =
    let%lwt tenant_languages = Settings.find_languages tenant_db in
    let bind_valid =
      flip CCOption.bind (fun lang ->
        if CCList.mem ~eq:Pool_common.Language.equal lang tenant_languages
        then Some lang
        else None)
    in
    match find_query_language url_parameters |> bind_valid with
    | Some language -> Lwt.return language
    | None ->
      (match contact_language contact |> bind_valid with
       | Some language -> Lwt.return language
       | None -> CCList.hd tenant_languages |> Lwt.return)
  in
  let filter handler req =
    let is_root = Http_utils.is_req_from_root_host req in
    let csrf = Sihl.Web.Csrf.find_exn req in
    let message =
      CCOption.bind (Sihl.Web.Flash.find_alert req) Pool_message.Collection.of_string
    in
    let find_user pool =
      Http_utils.user_from_session pool req
      >|> CCOption.map_or ~default:(Lwt.return Guest) (context_user_of_user pool)
    in
    let%lwt context =
      let* database_label = database_label_of_request is_root req |> Lwt_result.lift in
      let%lwt user = find_user database_label in
      let url_parameters = Utils.url_parameters_by_user req user in
      let%lwt language, guardian =
        let to_actor = Admin.id %> Guard.Uuid.actor_of Admin.Id.value in
        let combine roles = Lwt.return (Pool_common.Language.En, roles) in
        let request_language = request_language url_parameters database_label in
        match user with
        | Admin admin ->
          to_actor admin
          |> Guard.Persistence.ActorRole.permissions_of_actor database_label
          >|> combine
        | (Guest | Contact _) when is_root -> combine []
        | Contact contact ->
          let%lwt language = request_language (Some contact) in
          Lwt.return (language, [])
        | Guest ->
          let%lwt language = request_language None in
          Lwt.return (language, [])
      in
      let%lwt notifications = context_notification database_label user is_root in
      create
        ( url_parameters
        , language
        , database_label
        , message
        , csrf
        , user
        , guardian
        , notifications )
      |> Lwt.return_ok
    in
    match context with
    | Ok context -> context |> set req |> handler
    | Error _ ->
      let open Http_utils in
      let query_parameters = Utils.url_parameters_by_user req Guest in
      url_with_field_params query_parameters "/error" |> redirect_to
  in
  Rock.Middleware.create ~name:"tenant.context" ~filter
;;

(* Dynamically apply tenant middleware *)
let not_found () =
  let filter handler req =
    let is_root = Http_utils.is_req_from_root_host req in
    match is_root with
    | true -> handler req
    | false -> Middleware_tenant.web_filter handler req
  in
  Rock.Middleware.create ~name:"not_found.context" ~filter
;;
