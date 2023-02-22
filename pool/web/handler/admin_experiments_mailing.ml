module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let create_layout req = General.create_tenant_layout req
let experiment_id = HttpUtils.find_id Experiment.Id.of_string Field.Experiment

let experiment_path ?suffix id =
  [ Format.asprintf "/admin/experiments/%s" (Experiment.Id.value id) ]
  @ CCOption.map_or ~default:[] CCList.pure suffix
  |> CCString.concat "/"
;;

let index req =
  let open Utils.Lwt_result.Infix in
  let id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, experiment_path id)
    @@ let* experiment = Experiment.find database_label id in
       let%lwt mailings =
         Mailing.find_by_experiment database_label experiment.Experiment.id
       in
       Page.Admin.Mailing.index context experiment mailings
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let new_form req =
  let open Utils.Lwt_result.Infix in
  let id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err -> err, experiment_path id)
    @@ let* experiment = Experiment.find database_label id in
       let* is_bookable =
         Session.has_bookable_spots_for_experiments
           database_label
           experiment.Experiment.id
       in
       Page.Admin.Mailing.form
         ~fully_booked:(not is_bookable)
         context
         experiment
         (CCFun.flip Sihl.Web.Flash.find req)
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let result { Pool_context.database_label; _ } =
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.remove_empty_values
      ||> HttpUtils.format_request_boolean_values Field.[ RandomOrder |> show ]
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , experiment_path ~suffix:"mailings/create" experiment_id
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@ let* experiment = Experiment.find database_label experiment_id in
       let* distribution =
         Sihl.Web.Request.urlencoded_list Field.(array_key Distribution) req
         ||> Mailing.Distribution.of_urlencoded_list
       in
       let urlencoded =
         CCList.Assoc.set
           ~eq:( = )
           Field.(show Distribution)
           [ distribution ]
           urlencoded
       in
       let tags = Logger.req req in
       let events =
         let open CCResult in
         let open Cqrs_command.Mailing_command.Create in
         urlencoded
         |> HttpUtils.remove_empty_values
         |> decode
         >>= handle ~tags experiment
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
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
  let experiment_id = experiment_id req in
  let id = HttpUtils.find_id Mailing.Id.of_string Field.Mailing req in
  let result ({ Pool_context.database_label; _ } as context) =
    Utils.Lwt_result.map_error (fun err ->
      err, experiment_path ~suffix:"mailings" experiment_id)
    @@ let* mailing =
         Mailing.find database_label id
         >== fun m ->
         if edit
            && Ptime_clock.now () > Mailing.StartAt.value m.Mailing.start_at
         then Error Pool_common.Message.AlreadyStarted
         else Ok m
       in
       let* experiment = Experiment.find database_label experiment_id in
       (match edit with
        | false -> Page.Admin.Mailing.detail context experiment mailing
        | true ->
          Page.Admin.Mailing.form
            ~mailing
            context
            experiment
            (CCFun.flip Sihl.Web.Flash.find req))
       |> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path req
;;

let show = detail false
let edit = detail true

let update req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let id = HttpUtils.find_id Mailing.Id.of_string Field.Mailing req in
  let redirect_path =
    experiment_path
      ~suffix:
        ([ "mailings"; Mailing.Id.value id; "edit" ] |> CCString.concat "/")
      experiment_id
  in
  let result { Pool_context.database_label; _ } =
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.remove_empty_values
      ||> HttpUtils.format_request_boolean_values Field.[ RandomOrder |> show ]
    in
    Utils.Lwt_result.map_error (fun err ->
      err, redirect_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@ let* mailing = Mailing.find database_label id in
       let* distribution =
         Sihl.Web.Request.urlencoded_list Field.(array_key Distribution) req
         ||> Mailing.Distribution.of_urlencoded_list
       in
       let urlencoded =
         CCList.Assoc.set
           ~eq:( = )
           Field.(show Distribution)
           [ distribution ]
           urlencoded
       in
       let tags = Logger.req req in
       let events =
         let open CCResult in
         let open Cqrs_command.Mailing_command.Update in
         urlencoded |> decode >>= handle ~tags mailing
       in
       let handle events =
         let%lwt () =
           Lwt_list.iter_s (Pool_event.handle_event ~tags database_label) events
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
  let id = experiment_id req in
  let%lwt result =
    let open Utils.Lwt_result.Infix in
    let* ({ Pool_context.database_label; _ } as context) =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let* with_default_rate, mailing =
      Lwt_result.lift
      @@
      let open CCResult in
      Cqrs_command.Mailing_command.Overlaps.(
        urlencoded |> HttpUtils.remove_empty_values |> decode >>= handle)
    in
    let average_send, total =
      match with_default_rate with
      | true -> None, None
      | false ->
        Some (Mailing.per_minutes 5 mailing), Some (Mailing.total mailing)
    in
    let%lwt mailings = Mailing.find_overlaps database_label mailing in
    Page.Admin.Mailing.overlaps ?average_send ?total context id mailings
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
    let open Utils.Lwt_result.Infix in
    let* { Pool_context.language; _ } =
      Pool_context.find req |> Lwt_result.lift
    in
    let%lwt urlencoded = Sihl.Web.Request.to_urlencoded req in
    let invalid field = Pool_common.Message.(Invalid field) in
    let find_in_urlencoded read field =
      urlencoded
      |> CCList.assoc_opt ~eq:CCString.equal Field.(show field)
      |> CCFun.flip CCOption.bind CCList.head_opt
      |> CCFun.flip CCOption.bind (fun str ->
           try Some (read str) with
           | _ -> None)
      |> CCOption.to_result (invalid field)
    in
    let distribution =
      let open Mailing.Distribution in
      let open CCResult in
      let* field =
        Field.DistributionField |> find_in_urlencoded SortableField.read
      in
      let* order = Field.SortOrder |> find_in_urlencoded SortOrder.read in
      Ok (field, order)
    in
    Lwt.return_ok
    @@
    match distribution with
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
  let redirect_path = experiment_path ~suffix:"mailings" (experiment_id req) in
  let mailing_id = HttpUtils.find_id Mailing.Id.of_string Field.Mailing req in
  let result { Pool_context.database_label; _ } =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@ let* mailing = Mailing.find database_label mailing_id in
       let* events = command mailing |> Lwt_result.lift in
       let tags = Logger.req req in
       let%lwt () = Pool_event.handle_events ~tags database_label events in
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

module Access : sig
  include Helpers.AccessSig

  val add_condition : Rock.Middleware.t
  val search_info : Rock.Middleware.t
  val stop : Rock.Middleware.t
  val overlaps : Rock.Middleware.t
end = struct
  module MailingCommand = Cqrs_command.Mailing_command

  let experiment_effects =
    Middleware.Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let mailing_effects =
    Middleware.Guardian.id_effects Mailing.Id.of_string Field.Mailing
  ;;

  let index =
    Middleware.Guardian.validate_admin_entity [ `Read, `TargetEntity `Mailing ]
  ;;

  let create =
    [ MailingCommand.Create.effects ]
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let read =
    [ (fun id ->
        [ `Read, `Target (id |> Guard.Uuid.target_of Mailing.Id.value)
        ; `Read, `TargetEntity `Mailing
        ])
    ]
    |> mailing_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let update =
    [ MailingCommand.Update.effects ]
    |> mailing_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let delete =
    [ MailingCommand.Delete.effects ]
    |> mailing_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let add_condition =
    [ (fun id ->
        [ `Update, `Target (id |> Guard.Uuid.target_of Experiment.Id.value)
        ; `Update, `TargetEntity `Experiment
        ])
    ]
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let search_info =
    [ MailingCommand.Create.effects
    ; (fun _ -> [ `Update, `TargetEntity `Mailing ])
    ]
    |> experiment_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let stop =
    [ MailingCommand.Stop.effects ]
    |> mailing_effects
    |> Middleware.Guardian.validate_generic
  ;;

  let overlaps =
    MailingCommand.Overlaps.effects |> Middleware.Guardian.validate_admin_entity
  ;;
end
