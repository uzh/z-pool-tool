module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

let id req field encode =
  Sihl.Web.Router.param req @@ Pool_common.Message.Field.show field |> encode
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let id =
    Pool_common.Message.Field.(Experiment |> show)
    |> Sihl.Web.Router.param req
    |> Pool_common.Id.of_string
  in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Pool_common.Id.value id)
  in
  let result context =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, error_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* experiment = Experiment.find tenant_db id in
    let* waiting_list = Waiting_list.find_by_experiment tenant_db id in
    Page.Admin.Experiments.waiting_list waiting_list experiment context
    |> create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail req =
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    id req Pool_common.Message.Field.Experiment Pool_common.Id.of_string
  in
  let id =
    id req Pool_common.Message.Field.WaitingList Pool_common.Id.of_string
  in
  let error_path =
    Format.asprintf
      "/admin/experiments/%s/waiting-list"
      (Pool_common.Id.value experiment_id)
  in
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, error_path)
    @@ let* waiting_list = Waiting_list.find tenant_db id in
       let* sessions =
         Session.find_all_for_experiment tenant_db experiment_id
       in
       Page.Admin.WaitingList.detail waiting_list sessions experiment_id context
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let update req =
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    id req Pool_common.Message.Field.Experiment Pool_common.Id.of_string
  in
  let waiting_list_id =
    id req Pool_common.Message.Field.WaitingList Pool_common.Id.of_string
  in
  let redirect_path =
    let open Pool_common.Id in
    Format.asprintf
      "/admin/experiments/%s/waiting-list/%s"
      (value experiment_id)
      (value waiting_list_id)
  in
  let result context =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@
    let tenant_db = context.Pool_context.tenant_db in
    let* waiting_list = Waiting_list.find tenant_db waiting_list_id in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let events =
      let open Cqrs_command.Waiting_list_command in
      let open CCResult in
      urlencoded
      |> Update.decode
      >>= Update.handle waiting_list
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set
            ~success:[ Pool_common.Message.(Updated Field.WaitingList) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;

let assign_contact req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, waiting_list_id =
    let open Pool_common in
    ( id req Message.Field.Experiment Id.of_string
    , id req Message.Field.WaitingList Id.of_string )
  in
  let redirect_path =
    let open Pool_common.Id in
    Format.asprintf "/admin/experiments/%s/waiting-list" (experiment_id |> value)
  in
  let result context =
    Lwt_result.map_err (fun err ->
        ( err
        , Format.asprintf
            "%s/%s"
            redirect_path
            (Pool_common.Id.value waiting_list_id) ))
    @@
    let open Lwt_result.Syntax in
    let tenant_db = context.Pool_context.tenant_db in
    let* waiting_list = Waiting_list.find tenant_db waiting_list_id in
    let* session =
      let open Pool_common.Message in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      urlencoded
      |> CCList.assoc_opt ~eq:CCString.equal Field.(show Session)
      |> CCFun.flip CCOption.bind CCList.head_opt
      |> CCOption.to_result NoValue
      |> Lwt_result.lift
      >>= fun id -> id |> Pool_common.Id.of_string |> Session.find tenant_db
    in
    let%lwt already_enrolled =
      let open Lwt.Infix in
      Assignment.find_by_experiment_and_contact_opt
        tenant_db
        experiment_id
        waiting_list.Waiting_list.contact
      >|= CCOption.is_some
    in
    let* confirmation_email =
      let* language =
        let* default = Settings.default_language tenant_db in
        waiting_list.Waiting_list.contact.Contact.language
        |> CCOption.value ~default
        |> Lwt_result.return
      in
      let* subject =
        I18n.find_by_key
          tenant_db
          I18n.Key.ConfirmationWithoutSelfRegistrationSubject
          language
        >|= I18n.content
      in
      let* text =
        I18n.find_by_key
          tenant_db
          I18n.Key.ConfirmationWithoutSelfRegistrationText
          language
        >|= I18n.content
      in
      let session_text =
        Session.(
          to_email_text language session.start session.duration session.location)
      in
      Lwt_result.return Assignment.{ subject; text; language; session_text }
    in
    let events =
      Cqrs_command.Assignment_command.CreateFromWaitingList.(
        handle { session; waiting_list; already_enrolled })
        confirmation_email
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event tenant_db) events in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ HttpUtils.Message.set
            ~success:[ Pool_common.Message.(AssignmentCreated) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
