type config =
  { start : bool option
  ; rate_limit : int
  ; max_capacity : int
  }

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
  |> CCResult.map_err Pool_common.Utils.with_log_error
  |> Pool_common.Utils.get_or_failwith
;;

let sum = CCList.fold_left ( + ) 0

let count_of_rate ?(interval = Sihl.Time.OneMinute) rate =
  (* calculated number of invitations from the rate per hour to the specified
     interval *)
  let rate = max rate 0 in
  let interval_to_seconds =
    CCFun.(Sihl.Time.duration_to_span %> Ptime.Span.to_float_s)
  in
  CCFloat.(of_int rate / 3600. * (interval |> interval_to_seconds) |> round)
  |> CCInt.of_float
;;

let find_contacts_by_mailing pool { Mailing.id; distribution; _ } limit =
  let open Utils.Lwt_result.Infix in
  let%lwt ({ Experiment.id; _ } as experiment) =
    Experiment.find_of_mailing pool (id |> Mailing.Id.to_common)
    ||> get_or_failwith
  in
  let order_by =
    distribution |> CCOption.map Mailing.Distribution.get_order_element
  in
  let* contacts =
    Contact.find_filtered
      ?order_by
      ~limit:(max limit 0)
      pool
      (id |> Experiment.Id.to_common)
      experiment.Experiment.filter
  in
  (experiment, contacts) |> Lwt_result.return
;;

let calculate_mailing_limits ?interval pool_based_mailings =
  let open CCFun.Infix in
  let limit = read_int EmailRateLimit |> CCInt.to_float in
  let factor = read_int MaxCapacity |> CCInt.to_float in
  let max_total_invitations =
    CCFloat.(limit * (factor / 100.) |> to_int) |> count_of_rate ?interval
  in
  let total =
    pool_based_mailings
    |> CCList.fold_left
         (fun init (_, mailings) ->
           init
           :: (mailings
              |> CCList.map (fun ({ Mailing.rate; _ } : Mailing.t) ->
                   rate |> Mailing.Rate.value |> count_of_rate ?interval))
           |> sum)
         0
  in
  let reduce_factor = CCFloat.(of_int max_total_invitations / of_int total) in
  pool_based_mailings
  |> CCList.map (fun (pool, mailings) ->
       let limit_to_mailing =
         mailings
         |> CCList.map (fun ({ Mailing.rate; _ } as mailing : Mailing.t) ->
              let limit_per_mailing =
                (rate
                |> Mailing.Rate.value
                |> count_of_rate ?interval
                |> CCFloat.of_int)
                *. max reduce_factor 1.
                |> floor
                |> CCFloat.to_int
              in
              mailing, limit_per_mailing)
       in
       pool, limit_to_mailing)
;;

let match_invitations ?interval pools =
  let open CCFun.Infix in
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
  let%lwt pool_based_mailings =
    Lwt_list.map_s
      (fun pool ->
        Mailing.find_current pool
        >|> Lwt_list.filter_map_s (fun mailing ->
              let find_experiment { Mailing.id; _ } =
                Experiment.find_of_mailing pool (id |> Mailing.Id.to_common)
              in
              let has_spots { Experiment.id; _ } =
                Session.has_bookable_spots_for_experiments pool id
              in
              let validate = function
                | true -> Ok mailing
                | false -> Error Pool_common.Message.SessionFullyBooked
              in
              mailing
              |> find_experiment
              >>= has_spots
              >== validate
              ||> CCResult.to_opt)
        ||> fun m -> pool, m)
      pools
  in
  let create_events =
    let open Cqrs_command.Matcher_command.Run in
    let ok_or_log_error = function
      | Ok (pool, events) when CCList.is_empty events ->
        Logs.info (fun m ->
          m "Matcher: No actions for pool %a" Pool_database.Label.pp pool);
        None
      | Ok m -> Some m
      | Error err ->
        let open Pool_common in
        let (_ : Message.error) = Utils.with_log_error err in
        None
    in
    Lwt_list.filter_map_s (fun (pool, limited_mailings) ->
      let open Lwt_result.Syntax in
      let%lwt events =
        let* tenant = Pool_tenant.find_by_label pool in
        limited_mailings
        |> Lwt_list.map_s (fun (mailing, limit) ->
             find_contacts_by_mailing pool mailing limit
             >>= fun (experiment, contacts) ->
             let* create_message =
               Message_template.ExperimentInvitation.prepare tenant
             in
             { mailing; experiment; contacts; create_message }
             |> Lwt_result.return)
        ||> CCList.all_ok
      in
      let open CCResult in
      events
      >>= handle
      >|= (fun events -> pool, events)
      |> ok_or_log_error
      |> Lwt.return)
  in
  let handle_events =
    Lwt_list.iter_s (fun (pool, events) ->
      Logs.info (fun m ->
        m
          "Matcher: Sending %4d intivation emails for tenant %a"
          (count_mails events)
          Pool_database.Label.pp
          pool);
      Pool_event.handle_events pool events)
  in
  pool_based_mailings
  |> calculate_mailing_limits ?interval
  |> create_events
  >|> handle_events
;;

let stop_matcher : (unit -> unit) option ref = ref None

let start_matcher () =
  let open Utils.Lwt_result.Infix in
  let interval = Sihl.Time.TenMinutes in
  Logs.debug (fun m -> m "Start matcher");
  let scheduled_function () =
    Logs.info (fun m -> m "Matcher: Run");
    Pool_tenant.find_all ()
    ||> CCList.map (fun Pool_tenant.{ database_label; _ } -> database_label)
    >|> match_invitations ~interval
  in
  let schedule =
    Sihl.Schedule.create
      (Sihl.Schedule.Every interval)
      scheduled_function
      (Format.asprintf "matcher (Interval: %a)" Sihl.Time.pp_duration interval)
  in
  stop_matcher := Some (Sihl.Schedule.schedule schedule);
  Lwt.return ()
;;

let start () =
  Sihl.Configuration.require schema;
  if read_bool Run then start_matcher () else Lwt.return_unit
;;

let stop () =
  match !stop_matcher with
  | Some stop_matcher ->
    stop_matcher ();
    Lwt.return ()
  | None ->
    Logs.warn (fun m -> m "Can not stop schedule");
    Lwt.return ()
;;

let lifecycle =
  Sihl.Container.create_lifecycle
    "Matcher"
    ~dependencies:(fun () -> [ Sihl.Schedule.lifecycle ])
    ~start
    ~stop
;;

let register () =
  let configuration = Sihl.Configuration.make ~schema () in
  Sihl.Container.Service.create ~configuration lifecycle
;;
