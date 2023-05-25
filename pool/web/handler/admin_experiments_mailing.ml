module HttpUtils = Http_utils
module Message = HttpUtils.Message
module Field = Pool_common.Message.Field

let src = Logs.Src.create "handler.admin.experiments_mailing"
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
       >|> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
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
       >|> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
;;

let create req =
  let open Utils.Lwt_result.Infix in
  let experiment_id = experiment_id req in
  let result { Pool_context.database_label; _ } =
    let%lwt urlencoded =
      Sihl.Web.Request.to_urlencoded req
      ||> HttpUtils.remove_empty_values
      ||> HttpUtils.format_request_boolean_values
            Field.([ StartNow; RandomOrder ] |> CCList.map show)
    in
    Utils.Lwt_result.map_error (fun err ->
      ( err
      , experiment_path ~suffix:"mailings/create" experiment_id
      , [ HttpUtils.urlencoded_to_flash urlencoded ] ))
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* experiment = Experiment.find database_label experiment_id in
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
        [ Message.set ~success:[ Pool_common.Message.(Created Field.Mailing) ] ]
    in
    events |> Lwt_result.lift |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
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
       >|> create_layout req context
       >|+ Sihl.Web.Response.of_html
  in
  result |> HttpUtils.extract_happy_path ~src req
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
      ||> HttpUtils.format_request_boolean_values
            Field.([ StartNow; RandomOrder ] |> CCList.map show)
    in
    Utils.Lwt_result.map_error (fun err ->
      err, redirect_path, [ HttpUtils.urlencoded_to_flash urlencoded ])
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* mailing = Mailing.find database_label id in
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
        [ Message.set ~success:[ Pool_common.Message.(Updated Field.Mailing) ] ]
    in
    events |> Lwt_result.lift |>> handle
  in
  result |> HttpUtils.extract_happy_path_with_actions ~src req
;;

let search_info req =
  let id = experiment_id req in
  let result ({ Pool_context.database_label; _ } as context) =
    let open Utils.Lwt_result.Infix in
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
    |> HttpUtils.Htmx.html_to_plain_text_response
    |> Lwt.return_ok
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;

let add_condition req =
  let result { Pool_context.language; _ } =
    let open Mailing.Distribution in
    let open CCResult in
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
      let* field =
        Field.DistributionField |> find_in_urlencoded SortableField.read
      in
      let* order = Field.SortOrder |> find_in_urlencoded SortOrder.read in
      Ok (field, order)
    in
    distribution
    >|= Page.Admin.Mailing.distribution_form_field language
    >|= HttpUtils.Htmx.html_to_plain_text_response
    |> Lwt.return
  in
  result |> HttpUtils.Htmx.handle_error_message ~src req
;;

let disabler command success_handler req =
  let redirect_path = experiment_path ~suffix:"mailings" (experiment_id req) in
  let mailing_id = HttpUtils.find_id Mailing.Id.of_string Field.Mailing req in
  let result { Pool_context.database_label; _ } =
    let open Utils.Lwt_result.Infix in
    Utils.Lwt_result.map_error (fun err -> err, redirect_path)
    @@
    let tags = Pool_context.Logger.Tags.req req in
    let* mailing = Mailing.find database_label mailing_id in
    let* events = command mailing |> Lwt_result.lift in
    let%lwt () = Pool_event.handle_events ~tags database_label events in
    Http_utils.redirect_to_with_actions
      redirect_path
      [ Message.set
          ~success:[ Pool_common.Message.(success_handler Field.Mailing) ]
      ]
    |> Lwt_result.ok
  in
  result |> HttpUtils.extract_happy_path ~src req
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
  include module type of Helpers.Access

  val add_condition : Rock.Middleware.t
  val search_info : Rock.Middleware.t
  val stop : Rock.Middleware.t
  val overlaps : Rock.Middleware.t
end = struct
  include Helpers.Access
  module MailingCommand = Cqrs_command.Mailing_command
  module Guardian = Middleware.Guardian

  let experiment_effects =
    Guardian.id_effects Experiment.Id.of_string Field.Experiment
  ;;

  let combined_effects fcn req =
    let open HttpUtils in
    let experiment_id = find_id Experiment.Id.of_string Field.Experiment req in
    let mailing_id = find_id Mailing.Id.of_string Field.Mailing req in
    fcn experiment_id mailing_id
  ;;

  let index =
    Mailing.Guard.Access.index
    |> experiment_effects
    |> Guardian.validate_generic ~any_id:true
  ;;

  let create =
    MailingCommand.Create.effects
    |> experiment_effects
    |> Guardian.validate_generic
  ;;

  let read =
    Mailing.Guard.Access.read |> combined_effects |> Guardian.validate_generic
  ;;

  let update =
    MailingCommand.Update.effects
    |> combined_effects
    |> Guardian.validate_generic
  ;;

  let delete =
    MailingCommand.Delete.effects
    |> combined_effects
    |> Guardian.validate_generic
  ;;

  let add_condition =
    Experiment.Guard.Access.update
    |> experiment_effects
    |> Guardian.validate_generic
  ;;

  let search_info =
    (fun req ->
      Guard.ValidationSet.Or
        [ experiment_effects MailingCommand.Create.effects req
        ; combined_effects MailingCommand.Update.effects req
        ])
    |> Guardian.validate_generic
  ;;

  let stop =
    MailingCommand.Stop.effects |> combined_effects |> Guardian.validate_generic
  ;;

  let overlaps =
    MailingCommand.Overlaps.effects
    |> experiment_effects
    |> Guardian.validate_generic
  ;;
end
