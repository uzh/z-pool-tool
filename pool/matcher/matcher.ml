open CCFun.Infix

let src = Logs.Src.create "matcher.service"
let tags = Pool_database.(Logger.Tags.create root)

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

let count_of_rate_int ?(interval = Ptime.Span.of_int_s 60) rate =
  (* calculated number of invitations from the rate per hour to the specified
     interval *)
  let rate = max rate 0 in
  CCFloat.(of_int rate / 3600. * (interval |> Ptime.Span.to_float_s) |> round)
  |> CCInt.of_float
;;

let count_of_rate ?interval = Mailing.Rate.value %> count_of_rate_int ?interval

let find_contacts_by_mailing pool { Mailing.id; distribution; _ } limit =
  let open Utils.Lwt_result.Infix in
  let%lwt ({ Experiment.id; filter; _ } as experiment) =
    Experiment.find_of_mailing pool (id |> Mailing.Id.to_common)
    ||> get_or_failwith
  in
  let order_by =
    distribution |> CCOption.map Mailing.Distribution.get_order_element
  in
  let* contacts =
    Filter.find_filtered_contacts
      ?order_by
      ~limit:(max limit 0)
      pool
      (id |> Experiment.Id.to_common)
      filter
  in
  (experiment, contacts) |> Lwt_result.return
;;

let calculate_mailing_limits ?interval pool_based_mailings =
  let open CCList in
  let limit = read_int EmailRateLimit |> CCInt.to_float in
  let factor = read_int MaxCapacity |> CCInt.to_float in
  let max_total_invitations =
    CCFloat.(limit * (factor / 100.) |> to_int) |> count_of_rate_int ?interval
  in
  let total =
    pool_based_mailings
    |> fold_left
         (fun init (_, mailings) ->
           init
           :: (mailings
               >|= fun ({ Mailing.rate; _ } : Mailing.t) ->
               count_of_rate ?interval rate)
           |> sum)
         0
  in
  let reduce_factor =
    (* only allow a factor between 0 and 1 *)
    CCFloat.(of_int max_total_invitations / of_int total) |> min 1. |> max 0.
  in
  pool_based_mailings
  >|= fun (pool, mailings) ->
  let limit_to_mailing =
    mailings
    >|= fun ({ Mailing.rate; _ } as mailing : Mailing.t) ->
    let open CCFloat in
    let time_based_rate = rate |> count_of_rate ?interval |> of_int in
    let limit = time_based_rate *. reduce_factor |> floor |> to_int in
    mailing, limit
  in
  pool, limit_to_mailing
;;

let match_invitation_events ?interval pools =
  let open Utils.Lwt_result.Infix in
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
          >|- Pool_common.Utils.with_log_error ~level:Logs.Warning
          ||> CCResult.to_opt)
        ||> fun m -> pool, m)
      pools
  in
  let create_events =
    let open Cqrs_command.Matcher_command.Run in
    let ok_or_log_error = function
      | Ok (pool, events) when CCList.is_empty events ->
        Logs.info ~src (fun m ->
          m ~tags:(Pool_database.Logger.Tags.create pool) "No action");
        None
      | Ok m -> Some m
      | Error err ->
        let open Pool_common in
        let (_ : Message.error) = Utils.with_log_error ~tags err in
        None
    in
    let resend level pool tenant id limit =
      let%lwt experiment =
        Experiment.find_of_mailing pool (Mailing.Id.to_common id)
        ||> get_or_failwith
      in
      let%lwt invitations =
        Invitation.find_by_experiment_and_resent_count
          pool
          (experiment.Experiment.id, level)
          limit
        ||> get_or_failwith
      in
      invitations
      |> Lwt_list.map_s
           (fun ({ Invitation.contact; _ } as invitation : Invitation.t) ->
              let open Cqrs_command.Invitation_command.Resend in
              let%lwt invitation_mail =
                Message_template.ExperimentInvitation.create
                  tenant
                  experiment
                  contact
              in
              handle invitation_mail { invitation; experiment } |> Lwt.return)
      ||> CCResult.flatten_l
      >|+ CCList.flatten
    in
    Lwt_list.filter_map_s (fun (pool, limited_mailings) ->
      let%lwt events =
        let open Mailing.Process in
        let* tenant = Pool_tenant.find_by_label pool in
        limited_mailings
        |> Lwt_list.map_s
             (fun
                 (({ Mailing.id; process; _ } as mailing : Mailing.t), limit) ->
                match process with
                | NewInvitation ->
                  find_contacts_by_mailing pool mailing limit
                  >>= fun (experiment, contacts) ->
                  let%lwt create_message =
                    Message_template.ExperimentInvitation.prepare
                      tenant
                      experiment
                  in
                  { mailing; experiment; contacts; create_message }
                  |> handle
                  |> Lwt.return
                | ResendOnce ->
                  resend (Invitation.ResentCount.of_int 0) pool tenant id limit
                | ResendTwice ->
                  resend (Invitation.ResentCount.of_int 1) pool tenant id limit)
        ||> CCList.all_ok
        >|+ CCList.flatten
        >|+ fun events -> pool, events
      in
      events |> ok_or_log_error |> Lwt.return)
  in
  pool_based_mailings |> calculate_mailing_limits ?interval |> create_events
;;

let match_invitations ?interval pools =
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
          ~tags:(Pool_database.Logger.Tags.create pool)
          "Sending %4d intivation emails"
          (count_mails events));
      Pool_event.handle_events pool events)
  in
  match_invitation_events ?interval pools >|> handle_events
;;

let start_matcher () =
  let open Utils.Lwt_result.Infix in
  let open Schedule in
  let interval = Ptime.Span.of_int_s (5 * 60) in
  let periodic_fcn () =
    Logs.debug ~src (fun m ->
      m ~tags:Pool_database.(Logger.Tags.create root) "Run");
    Pool_tenant.find_all ()
    ||> CCList.map (fun Pool_tenant.{ database_label; _ } -> database_label)
    >|> match_invitations ~interval
  in
  let schedule =
    create
      "matcher"
      (Every (interval |> ScheduledTimeSpan.of_span))
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
