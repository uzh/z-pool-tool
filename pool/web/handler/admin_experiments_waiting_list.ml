module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment

let index req =
  let open Utils.Lwt_result.Infix in
  let id = experiment_id req in
  let error_path =
    Format.asprintf "/admin/experiments/%s" (Experiment.Id.value id)
  in
  let result context =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@
    let database_label = context.Pool_context.database_label in
    let* experiment = Experiment.find database_label id in
    let query =
      let open Waiting_list in
      Query.from_request ~searchable_by ~sortable_by req
    in
    let* waiting_list =
      Waiting_list.find_by_experiment
        ~query
        database_label
        experiment.Experiment.id
    in
    Page.Admin.Experiments.waiting_list waiting_list context
    |> create_layout req context
    >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let detail req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, id =
    ( experiment_id req
    , HttpUtils.find_id Pool_common.Id.of_string Field.WaitingList req )
  in
  let error_path =
    Format.asprintf
      "/admin/experiments/%s/waiting-list"
      (Experiment.Id.value experiment_id)
  in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, error_path)
    @@ let* waiting_list = Waiting_list.find database_label id in
       let* sessions =
         Session.find_all_for_experiment database_label experiment_id
         >|+ Session.group_and_sort
       in
       let flash_fetcher key = Sihl.Web.Flash.find key req in
       Page.Admin.WaitingList.detail
         waiting_list
         sessions
         experiment_id
         context
         flash_fetcher
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let update req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, waiting_list_id =
    ( experiment_id req
    , HttpUtils.find_id Pool_common.Id.of_string Field.WaitingList req )
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/waiting-list/%s"
      (Experiment.Id.value experiment_id)
      (Pool_common.Id.value waiting_list_id)
  in
  let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      err, redirect_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* waiting_list = Waiting_list.find database_label waiting_list_id in
    let events =
      let open Cqrs_command.Waiting_list_command in
      let open CCResult in
      urlencoded
      |> Update.decode
      >>= Update.handle ~tags waiting_list
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set
            ~success:[ Pool_common.Message.(Updated Field.WaitingList) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let assign_contact req =
  let open Utils.Lwt_result.Infix in
  let experiment_id, waiting_list_id =
    ( experiment_id req
    , HttpUtils.find_id Pool_common.Id.of_string Field.WaitingList req )
  in
  let redirect_path =
    let open Experiment.Id in
    Format.asprintf "/admin/experiments/%s/waiting-list" (experiment_id |> value)
  in
  let result { Pool_context.database_label; _ } =
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , Format.asprintf
          "%s/%s"
          redirect_path
          (Pool_common.Id.value waiting_list_id) ))
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* { Pool_context.Tenant.tenant; _ } =
      Pool_context.Tenant.find req |> Lwt_result.lift
    in
    let* waiting_list = Waiting_list.find database_label waiting_list_id in
    let* session =
      let open Pool_common.Message in
      let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
      urlencoded
      |> CCList.assoc_opt ~eq:CCString.equal Field.(show Session)
      |> CCFun.flip CCOption.bind CCList.head_opt
      |> CCOption.to_result NoValue
      |> Lwt_result.lift
      >>= fun id ->
      id |> Pool_common.Id.of_string |> Session.find database_label
    in
    let%lwt already_enrolled =
      let open Utils.Lwt_result.Infix in
      Assignment.find_by_experiment_and_contact_opt
        database_label
        experiment_id
        waiting_list.Waiting_list.contact
      ||> CCList.is_empty
      ||> not
    in
    let* confirmation_email =
      let contact = waiting_list.Waiting_list.contact in
      let* language =
        let* default = Settings.default_language database_label in
        contact.Contact.language |> CCOption.value ~default |> Lwt_result.return
      in
      Message_template.AssignmentConfirmation.create
        database_label
        language
        tenant
        session
        contact
    in
    let events =
      let open Cqrs_command.Assignment_command.CreateFromWaitingList in
      (handle ~tags { session; waiting_list; already_enrolled })
        confirmation_email
      |> Lwt_result.lift
    in
    let handle events =
      let%lwt () =
        Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
      in
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

module Access : sig
  include Helpers.AccessSig

  val assign : Rock.Middleware.t
end = struct
  module WaitingListCommand = Cqrs_command.Waiting_list_command
  module Field = Pool_common.Message.Field

  let waiting_list_effects =
    Middleware.Guardian.id_effects Pool_common.Id.of_string Field.WaitingList
  ;;

  let index =
    Middleware.Guardian.validate_admin_entity
      [ `Read, `TargetEntity `WaitingList ]
  ;;

  let create =
    WaitingListCommand.Create.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;

  let read =
    [ (fun id ->
        [ `Read, `Target (id |> Guard.Uuid.target_of Pool_common.Id.value)
        ; `Read, `TargetEntity `WaitingList
        ])
    ]
    |> waiting_list_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let update =
    [ WaitingListCommand.Update.effects ]
    |> waiting_list_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let delete =
    [ WaitingListCommand.Destroy.effects ]
    |> waiting_list_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let assign =
    Cqrs_command.Assignment_command.CreateFromWaitingList.effects
    |> Middleware.Guardian.validate_admin_entity
  ;;
end
