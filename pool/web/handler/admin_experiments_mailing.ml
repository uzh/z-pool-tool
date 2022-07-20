module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let create_layout req = General.create_tenant_layout `Admin req

let id req field encode =
  Sihl.Web.Router.param req @@ Field.show field |> encode
;;

let experiment_path ?suffix experiment_id =
  [ Format.asprintf "/admin/experiments/%s" (Pool_common.Id.value experiment_id)
  ]
  @ CCOption.map_or ~default:[] CCList.pure suffix
  |> CCString.concat "/"
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Field.Experiment Pool_common.Id.of_string in
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, experiment_path experiment_id)
    @@ let* experiment = Experiment.find tenant_db experiment_id in
       let%lwt mailings =
         Mailing.find_by_experiment tenant_db experiment.Experiment.id
       in
       Page.Admin.Mailing.index context experiment mailings
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Field.Experiment Pool_common.Id.of_string in
  let result ({ Pool_context.tenant_db; _ } as context) =
    Lwt_result.map_error (fun err -> err, experiment_path experiment_id)
    @@
    let open Lwt_result.Syntax in
    let* experiment = Experiment.find tenant_db experiment_id in
    Page.Admin.Mailing.form
      context
      experiment
      (CCFun.flip Sihl.Web.Flash.find req)
    |> create_layout req context
    >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Field.Experiment Pool_common.Id.of_string in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Lwt_result.map_error (fun err ->
        ( err
        , experiment_path ~suffix:"mailings/create" experiment_id
        , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@ let* experiment = Experiment.find tenant_db experiment_id in
       let* distribution =
         Sihl.Web.Request.urlencoded_list
           Pool_common.Message.Field.(array_key Distribution)
           req
         ||> Mailing.Distribution.of_urlencoded_list
       in
       let urlencoded =
         CCList.Assoc.set
           ~eq:( = )
           Pool_common.Message.Field.(show Distribution)
           [ distribution ]
           urlencoded
       in
       let events =
         let open CCResult in
         let open Cqrs_command.Mailing_command.Create in
         urlencoded
         |> HttpUtils.remove_empty_values
         |> decode
         >>= handle experiment
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           (experiment_path ~suffix:"mailings" experiment_id)
           [ Message.set
               ~success:[ Pool_common.Message.(Created Field.Mailing) ]
           ]
       in
       events |> Lwt_result.lift |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let detail edit req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Field.Experiment Pool_common.Id.of_string in
  let id = id req Field.Mailing Mailing.Id.of_string in
  let result ({ Pool_context.tenant_db; _ } as context) =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err ->
        err, experiment_path ~suffix:"mailings" experiment_id)
    @@ let* mailing =
         Mailing.find tenant_db id
         >== fun m ->
         if edit
            && Ptime_clock.now () > Mailing.StartAt.value m.Mailing.start_at
         then Error Pool_common.Message.AlreadyStarted
         else Ok m
       in
       let* experiment = Experiment.find tenant_db experiment_id in
       (match edit with
       | false -> Page.Admin.Mailing.detail context experiment mailing
       | true ->
         Page.Admin.Mailing.form
           ~mailing
           context
           experiment
           (CCFun.flip Sihl.Web.Flash.find req))
       |> create_layout req context
       >|= Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show = detail false
let edit = detail true

let update req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = id req Field.Experiment Pool_common.Id.of_string in
  let id = id req Field.Mailing Mailing.Id.of_string in
  let redirect_path =
    experiment_path
      ~suffix:
        ([ "mailings"; Mailing.Id.value id; "edit" ] |> CCString.concat "/")
      experiment_id
  in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req ||> HttpUtils.remove_empty_values
    in
    Lwt_result.map_error (fun err ->
        err, redirect_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@ let* mailing = Mailing.find tenant_db id in
       let* distribution =
         Sihl.Web.Request.urlencoded_list
           Pool_common.Message.Field.(array_key Distribution)
           req
         ||> Mailing.Distribution.of_urlencoded_list
       in
       let urlencoded =
         CCList.Assoc.set
           ~eq:( = )
           Pool_common.Message.Field.(show Distribution)
           [ distribution ]
           urlencoded
       in
       let events =
         let open CCResult in
         let open Cqrs_command.Mailing_command.Update in
         urlencoded |> decode >>= handle mailing
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event tenant_db) events
         in
         Http_utils.redirect_to_with_actions
           redirect_path
           [ Message.set
               ~success:[ Pool_common.Message.(Updated Field.Mailing) ]
           ]
       in
       events |> Lwt_result.lift |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions req
;;

let search_info req =
  let experiment_id = id req Field.Experiment Pool_common.Id.of_string in
  let%lwt result =
    let open Lwt_result.Syntax in
    let* ({ Pool_context.tenant_db; _ } as context) =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* with_default_rate, mailing =
      Lwt_result.lift
      @@
      let open CCResult in
      Cqrs_command.Mailing_command.Overlaps.(
        urlencoded |> HttpUtils.remove_empty_values |> decode >>= create)
    in
    let average_send, total =
      match with_default_rate with
      | true -> None, None
      | false ->
        Some (Mailing.per_minutes 5 mailing), Some (Mailing.total mailing)
    in
    let%lwt mailings = Mailing.find_overlaps tenant_db mailing in
    Page.Admin.Mailing.overlaps
      ?average_send
      ?total
      context
      experiment_id
      mailings
    |> Lwt.return_ok
  in
  Lwt.return
  @@
  match result with
  | Ok mailings -> mailings |> HttpUtils.html_to_plain_text_response
  | Error _ -> Rock.Response.make ()
;;

let add_condition req =
  let%lwt result =
    let open Lwt_result.Syntax in
    let* { Pool_context.language; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let field =
      let open CCResult in
      let error = Pool_common.Message.(Invalid Field.DistributionField) in
      urlencoded
      |> CCList.assoc_opt
           ~eq:CCString.equal
           Pool_common.Message.Field.(show DistributionField)
      |> CCFun.flip CCOption.bind CCList.head_opt
      |> CCFun.flip CCOption.bind (fun str ->
             try Some (Pool_common.Message.Field.read str) with
             | _ -> None)
      |> CCOption.to_result error
      >>= (fun field ->
            if CCList.mem
                 ~eq:Pool_common.Message.Field.equal
                 field
                 Mailing.Distribution.sortable_fields
            then Ok field
            else Error error)
      >|= fun field -> Mailing.Distribution.(field, SortOrder.default)
    in
    Lwt.return_ok
    @@
    match field with
    | Ok field -> Page.Admin.Mailing.distribution_form_field language field
    | Error error ->
      Tyxml.Html.(
        div
          ~a:[ a_class [ "error" ] ]
          [ txt Pool_common.Utils.(error_to_string language error) ])
  in
  Lwt.return
  @@
  match result with
  | Ok html -> html |> HttpUtils.html_to_plain_text_response
  | Error _ -> Rock.Response.make ()
;;

let disabler command success_handler req =
  let redirect_path =
    experiment_path
      ~suffix:"mailings"
      (id req Field.Experiment Pool_common.Id.of_string)
  in
  let mailing_id = id req Field.Mailing Mailing.Id.of_string in
  let result { Pool_context.tenant_db; _ } =
    let open Lwt_result.Syntax in
    Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* mailing = Mailing.find tenant_db mailing_id in
       let* events = command mailing |> Lwt_result.lift in
       let%lwt () = Pool_event.handle_events tenant_db events in
       Http_utils.redirect_to_with_actions
         redirect_path
         [ Message.set
             ~success:[ Pool_common.Message.(success_handler Field.Mailing) ]
         ]
       |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path req
;;

let stop =
  disabler Cqrs_command.Mailing_command.Stop.handle Pool_common.Message.stopped
;;

let delete =
  disabler
    Cqrs_command.Mailing_command.Delete.handle
    Pool_common.Message.deleted
;;
