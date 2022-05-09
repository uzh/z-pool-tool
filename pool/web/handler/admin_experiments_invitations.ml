module HttpUtils = Http_utils
module Message = HttpUtils.Message

let create_layout req = General.create_tenant_layout `Admin req

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
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, error_path)
    @@ let* experiment_invitations =
         Experiment_type.find_invitations tenant_db id
       in
       let experiment = experiment_invitations.Experiment_type.experiment in
       let%lwt filtered_subjects =
         Subject.find_filtered tenant_db experiment.Experiment.filter ()
       in
       Page.Admin.Experiments.invitations
         experiment_invitations
         filtered_subjects
         context
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    Pool_common.Message.Field.(Experiment |> show)
    |> Sihl.Web.Router.param req
    |> Pool_common.Id.of_string
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (Pool_common.Id.value experiment_id)
  in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@ let%lwt subject_ids =
         let open Lwt.Infix in
         Sihl.Web.Request.urlencoded_list
           Pool_common.Message.Field.(Subjects |> array_key)
           req
         >|= CCList.map Pool_common.Id.of_string
       in
       let* experiment = Experiment.find tenant_db experiment_id in
       let* subjects =
         let find_missing subjects =
           let retrieved_ids = CCList.map Subject.id subjects in
           CCList.fold_left
             (fun missing id ->
               match CCList.mem ~eq:Pool_common.Id.equal id retrieved_ids with
               | true -> missing
               | false -> CCList.cons id missing)
             []
             subject_ids
         in
         let%lwt subjects = Subject.find_multiple tenant_db subject_ids in
         Lwt_result.lift
         @@
         match CCList.length subject_ids == CCList.length subjects with
         | true -> Ok subjects
         | false ->
           find_missing subjects
           |> CCList.map Pool_common.Id.value
           |> fun ids ->
           Error Pool_common.Message.(NotFoundList (Field.Subjects, ids))
       in
       let* default_language = Settings.default_language tenant_db in
       let%lwt events =
         let event subject =
           Cqrs_command.Invitation_command.Create.(
             handle { experiment; subject } default_language |> Lwt_result.lift)
         in
         Lwt_list.map_s event subjects
       in
       let handle events =
         let%lwt (_ : unit list) =
           Lwt_list.map_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set
               ~success:[ Pool_common.Message.(SentList Field.Invitations) ]
           ]
       in
       events
       |> CCResult.flatten_l
       |> Lwt_result.lift
       >|= CCList.flatten
       |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;

let resend req =
  let open Utils.Lwt_result.Infix in
  let experiment_id =
    Sihl.Web.Router.param req Pool_common.Message.Field.(Experiment |> show)
    |> Pool_common.Id.of_string
  in
  let redirect_path =
    Format.asprintf
      "/admin/experiments/%s/invitations"
      (Pool_common.Id.value experiment_id)
  in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_err (fun err -> err, redirect_path)
    @@
    let id = Sihl.Web.Router.param req "id" |> Pool_common.Id.of_string in
    let* invitation = Invitation.find tenant_db id in
    let* experiment = Experiment.find tenant_db experiment_id in
    let* default_language = Settings.default_language tenant_db in
    let events =
      Cqrs_command.Invitation_command.Resend.handle
        Invitation.{ invitation; experiment }
        default_language
      |> Lwt.return
    in
    let handle events =
      let%lwt (_ : unit list) =
        Lwt_list.map_s (Pool_event.handle_event tenant_db) events
      in
      Http_utils.redirect_to_with_actions
        redirect_path
        [ Message.set
            ~success:[ Pool_common.Message.(SentList Field.Invitations) ]
        ]
    in
    events |>> handle
  in
  result |> HttpUtils.extract_happy_path req
;;
