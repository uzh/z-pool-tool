open CCFun.Infix
open Utils.Lwt_result.Infix

let src = Logs.Src.create "matcher.service"
let tags = Database.(Logger.Tags.create Pool.Root.label)

type config =
  { start : bool option
  ; rate_limit : int
  ; max_capacity : int
  }
[@@warning "-69"]

let config start rate_limit max_capacity = { start; rate_limit; max_capacity }

type env =
  | Run
  | EmailRateLimit
  | MaxCapacity

let to_string = function
  | Run -> "MATCHER_RUN"
  | EmailRateLimit -> "EMAIL_RATE_LIMIT"
  | MaxCapacity -> "MATCHER_MAX_CAPACITY"
;;

let read_variable fcn env =
  fcn (env |> to_string)
  |> CCOption.get_exn_or
       (Format.asprintf "Variable not defined: %s" (env |> to_string))
;;

let read_int = read_variable Sihl.Configuration.read_int
let read_bool = read_variable Sihl.Configuration.read_bool

let schema =
  let open Conformist in
  make
    Field.
      [ Conformist.optional
          (bool
             ~meta:"If set to false, the matcher will not be executed."
             ~default:(Sihl.Configuration.is_production ())
             (Run |> to_string))
      ; int
          ~meta:"Rate limit of the mail server to external mail addresses"
          ~validator:(fun m ->
            if m >= 0
            then None
            else Some "Rate limit cannot have a value below zero.")
          (EmailRateLimit |> to_string)
      ; int
          ~meta:"maximum percentage of the rate limit used for invitations"
          ~validator:(fun m ->
            if m >= 0 && m <= 100 then None else Some "Not a percentage value.")
          (MaxCapacity |> to_string)
      ]
    config
;;

let get_or_failwith element =
  element
  |> CCResult.map_err (Pool_common.Utils.with_log_error ~src ~tags)
  |> Pool_common.Utils.get_or_failwith
;;

let sum = CCList.fold_left ( + ) 0

let for_interval interval rate =
  (* calculated number from the rate per hour to the specified interval *)
  let rate = max rate 0. in
  CCFloat.(rate / 3600. * (interval |> Ptime.Span.to_float_s))
;;

let experiment_has_bookable_spots
      database_label
      { Experiment.id; online_experiment; _ }
  =
  let open Utils.Lwt_result.Infix in
  let open Session in
  match CCOption.is_some online_experiment with
  | false ->
    find_upcoming_for_experiment database_label id
    ||> CCList.filter (fun session ->
      CCOption.is_none session.follow_up_to && not (is_fully_booked session))
    ||> CCList.is_empty
    ||> not
  | true ->
    Time_window.find_upcoming_by_experiment database_label id
    ||> (function
     | None -> false
     | Some { Time_window.max_participants; participant_count; _ } ->
       max_participants
       |> CCOption.map_or ~default:true (fun max_participants ->
         ParticipantAmount.value max_participants
         > ParticipantCount.value participant_count))
;;

let sort_contacts contacts =
  match Sihl.Configuration.is_test () with
  | false -> contacts
  | true ->
    CCList.stable_sort
      (fun c1 c2 -> Contact.(Id.compare (id c1) (id c2)))
      contacts
;;

let find_contacts_by_mailing pool { Mailing.id; distribution; _ } limit =
  let open Utils.Lwt_result.Infix in
  let%lwt ({ Experiment.id; filter; invitation_reset_at; _ } as experiment) =
    Experiment.find_of_mailing pool (id |> Mailing.Id.to_common)
    ||> get_or_failwith
  in
  let use_case =
    let id = id |> Experiment.Id.to_common in
    match invitation_reset_at with
    | Some reset_at ->
      Filter.MatcherReset (id, Experiment.InvitationResetAt.value reset_at)
    | None -> Filter.Matcher id
  in
  let order_by =
    distribution |> CCOption.map Mailing.Distribution.get_order_element
  in
  let* contacts =
    let limit = max limit 0 in
    Filter.find_filtered_contacts ?order_by ~limit pool use_case filter
  in
  (experiment, contacts, use_case) |> Lwt_result.return
;;

let calculate_mailing_limits
      interval
      (pool_based_mailings : ('a * Mailing.Status.status list) list)
  =
  let open CCList in
  let open CCFloat in
  let rate_limit = read_int EmailRateLimit |> CCInt.to_float in
  let factor = read_int MaxCapacity |> CCInt.to_float in
  let max_total_invitations =
    rate_limit * (factor / 100.) |> for_interval interval
  in
  let total =
    let open Mailing.Status in
    pool_based_mailings
    |> fold_left
         (fun init (_, status) ->
            init :: (status >|= to_handle %> ToHandle.value) |> sum)
         0
  in
  let reduce_factor =
    (* only allow a factor between 0 and 1 *)
    max_total_invitations / of_int total |> min 1. |> max 0.
  in
  pool_based_mailings
  >|= fun (pool, mailing_status) ->
  let limit_to_mailing =
    mailing_status
    >|= fun { Mailing.Status.mailing; to_handle; _ } ->
    let open CCFloat in
    let to_handle = Mailing.Status.ToHandle.value to_handle |> of_int in
    let limit = to_handle *. reduce_factor |> floor |> to_int in
    mailing, limit
  in
  pool, limit_to_mailing
;;

let notify_all_invited pool tenant experiment =
  let open Experiment in
  match MatcherNotificationSent.value experiment.matcher_notification_sent with
  | true -> Lwt.return []
  | false ->
    let%lwt email_event =
      Experiment.find_admins_to_notify_about_invitations
        pool
        experiment.Experiment.id
      >|> Lwt_list.map_s (fun admin ->
        admin
        |> Message_template.MatcherNotification.create
             tenant
             Pool_common.Language.En
             experiment)
      ||> Email.bulksent
      ||> Pool_event.email
    in
    let updated =
      { experiment with
        matcher_notification_sent = MatcherNotificationSent.create true
      }
    in
    let experiment_event =
      Updated (experiment, updated) |> Pool_event.experiment
    in
    Lwt.return [ email_event; experiment_event ]
;;

let events_of_mailings =
  let ok_or_log_error = function
    | Ok (pool, events) when CCList.is_empty events ->
      Logs.info ~src (fun m ->
        m ~tags:(Database.Logger.Tags.create pool) "No action");
      None
    | Ok m -> Some m
    | Error err ->
      let open Pool_common in
      let (_ : Pool_message.Error.t) = Utils.with_log_error ~tags err in
      None
  in
  Lwt_list.filter_map_s (fun (pool, limited_mailings) ->
    let open Lwt_result.Syntax in
    let%lwt events =
      let* tenant = Pool_tenant.find_by_label pool in
      limited_mailings
      |> Lwt_list.map_s (fun (mailing, limit) ->
        find_contacts_by_mailing pool mailing limit
        >>= fun (experiment, contacts, use_case) ->
        match contacts with
        | [] -> notify_all_invited pool tenant experiment |> Lwt_result.ok
        | contacts ->
          let open Cqrs_command.Invitation_command in
          let contacts = sort_contacts contacts in
          let%lwt create_message =
            Message_template.ExperimentInvitation.prepare tenant experiment
          in
          let create_new contacts =
            Create.(
              handle
                ~tags
                { experiment
                ; mailing = Some mailing
                ; contacts
                ; invited_contacts = []
                ; create_message
                })
          in
          let resend_existing invitations =
            invitations
            |> CCList.map (fun invitation ->
              Resend.(
                handle
                  ~tags
                  ~mailing_id:mailing.Mailing.id
                  create_message
                  invitation))
            |> CCList.all_ok
            |> CCResult.map CCList.flatten
          in
          (match use_case with
           | Filter.MatchesFilter -> failwith "Invalid use case"
           | Filter.Matcher _ -> create_new contacts |> Lwt_result.lift
           | Filter.MatcherReset _ ->
             contacts
             |> Lwt_list.fold_left_s
                  (fun (invitations, contacts) contact ->
                     contact
                     |> Contact.id
                     |> Invitation.find_by_contact_and_experiment_opt
                          pool
                          experiment.Experiment.id
                     |> Lwt.map (function
                       | None -> invitations, contacts @ [ contact ]
                       | Some invitation ->
                         invitations @ [ invitation ], contacts))
                  ([], [])
             >|> fun (invitations, contacts) ->
             let* resend_events =
               resend_existing invitations |> Lwt_result.lift
             in
             let* create_events = create_new contacts |> Lwt_result.lift in
             Lwt_result.return (create_events @ resend_events)))
      ||> CCList.all_ok
      >|+ CCList.flatten
    in
    let open CCResult in
    events >|= CCPair.make pool |> ok_or_log_error |> Lwt.return)
;;

let create_invitation_events interval pools =
  let%lwt pool_based_mailings =
    Lwt_list.map_s
      (fun pool ->
         Mailing.Status.find_current pool interval
         >|> Lwt_list.filter_map_s
               (fun ({ Mailing.Status.mailing; _ } as status) ->
                  let find_experiment { Mailing.id; _ } =
                    Experiment.find_of_mailing pool (id |> Mailing.Id.to_common)
                  in
                  let has_spots = experiment_has_bookable_spots pool in
                  let validate = function
                    | true -> Ok status
                    | false -> Error Pool_message.Error.SessionFullyBooked
                  in
                  mailing
                  |> find_experiment
                  |>> has_spots
                  >== validate
                  >|- Pool_common.Utils.with_log_error ~level:Logs.Warning
                  ||> CCResult.to_opt)
         ||> fun m -> pool, m)
      pools
  in
  pool_based_mailings |> calculate_mailing_limits interval |> events_of_mailings
;;

let match_invitations interval pools =
  let open Utils.Lwt_result.Infix in
  let count_mails =
    CCList.filter_map
      (let open Pool_event in
       function[@warning "-4"]
       (* TODO: Account based internal/external email count *)
       | Email (Email.Sent _) -> Some 1
       | Email (Email.BulkSent mails) -> Some (CCList.length mails)
       | _ -> None)
    %> sum
  in
  let handle_events =
    Lwt_list.iter_s (fun (pool, events) ->
      Logs.info ~src (fun m ->
        m
          ~tags:(Database.Logger.Tags.create pool)
          "Sending %4d intivation emails"
          (count_mails events));
      Pool_event.handle_system_events pool events)
  in
  create_invitation_events interval pools >|> handle_events
;;

let start_matcher () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s (5 * 60) in
  let periodic_fcn () =
    Logs.debug ~src (fun m ->
      m ~tags:Database.(Logger.Tags.create Pool.Root.label) "Run");
    Database.(Pool.Tenant.all ~status:Status.[ Active ] ())
    |> match_invitations interval
  in
  let schedule =
    create
      "matcher"
      (Every (interval |> ScheduledTimeSpan.of_span))
      None
      periodic_fcn
  in
  Schedule.add_and_start schedule
;;

let start () =
  Sihl.Configuration.require schema;
  if read_bool Run then start_matcher () else Lwt.return_unit
;;

let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "Matcher"
    ~dependencies:(fun () -> [ Schedule.lifecycle ])
    ~start
    ~stop
;;

let register () =
  let configuration = Sihl.Configuration.make ~schema () in
  Sihl.Container.Service.create ~configuration lifecycle
;;
