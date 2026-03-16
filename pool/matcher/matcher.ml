open CCFun.Infix
open Utils.Lwt_result.Infix

let src = Logs.Src.create "matcher.service"
let tags = Database.(Logger.Tags.create Pool.Root.label)

type env = Run

let to_string = function
  | Run -> "MATCHER_RUN"
;;

let read_bool env =
  Sihl.Configuration.read_bool (env |> to_string)
  |> CCOption.get_exn_or (Format.asprintf "Variable not defined: %s" (env |> to_string))
;;

let schema =
  let open Conformist in
  make
    Field.
      [ Conformist.optional
          (bool
             ~meta:"If set to false, the matcher will not be executed."
             ~default:(Sihl.Configuration.is_production ())
             (Run |> to_string))
      ]
    CCFun.id
;;

let get_or_failwith element =
  element
  |> CCResult.map_err (Pool_common.Utils.with_log_error ~src ~tags)
  |> Pool_common.Utils.get_or_failwith
;;

let sum = CCList.fold_left ( + ) 0

let experiment_has_bookable_spots database_label { Experiment.id; online_experiment; _ } =
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
    CCList.stable_sort (fun c1 c2 -> Contact.(Id.compare (id c1) (id c2))) contacts
;;

let find_contacts_by_mailing pool { Mailing.id; distribution; _ } limit =
  let open Utils.Lwt_result.Infix in
  let%lwt ({ Experiment.id; filter; _ } as experiment) =
    Experiment.find_of_mailing pool (id |> Mailing.Id.to_common) ||> get_or_failwith
  in
  let use_case = Filter.Matcher (Experiment.Id.to_common id) in
  let order_by = distribution |> CCOption.map Mailing.Distribution.get_order_element in
  let* contacts =
    let limit = max limit 0 in
    Filter.find_filtered_contacts ?order_by ~limit pool use_case filter
  in
  (experiment, contacts, use_case) |> Lwt_result.return
;;

let calculate_mailing_limits remaining_capacity statuses =
  (* Distribute [remaining_capacity] across mailings proportionally to their
     requested amounts (to_handle). If the total requested <= remaining_capacity
     every mailing gets its full amount; otherwise each gets a proportional share. *)
  let open CCList in
  let open CCFloat in
  let total =
    statuses
    |> fold_left
         (fun acc s -> acc +. (Mailing.Status.(to_handle s |> ToHandle.value) |> of_int))
         0.
  in
  let capacity = of_int remaining_capacity in
  let reduce_factor = if total <= 0. then 0. else capacity /. total |> min 1. |> max 0. in
  statuses
  >|= fun { Mailing.Status.mailing; to_handle; _ } ->
  let to_handle = Mailing.Status.ToHandle.value to_handle |> of_int in
  let limit = to_handle *. reduce_factor |> floor |> to_int in
  mailing, limit
;;

let notify_all_invited pool tenant experiment =
  let open Experiment in
  match MatcherNotificationSent.value experiment.matcher_notification_sent with
  | true -> Lwt.return []
  | false ->
    let%lwt email_event =
      Experiment.find_admins_to_notify_about_invitations pool experiment.Experiment.id
      >|> Lwt_list.map_s
            (Message_template.MatcherNotification.create
               tenant
               Pool_common.Language.En
               experiment)
      ||> Email.bulksent_opt %> Pool_event.(map email)
    in
    let updated =
      { experiment with matcher_notification_sent = MatcherNotificationSent.create true }
    in
    let experiment_event = Updated (experiment, updated) |> Pool_event.experiment in
    Lwt.return (experiment_event :: email_event)
;;

let events_of_mailings ?invitation_ids pool limited_mailings =
  let tags = Database.Logger.Tags.create pool in
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
              ?ids:invitation_ids
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
            Resend.(handle ~tags ~mailing_id:mailing.Mailing.id create_message invitation))
          |> CCList.all_ok
          |> CCResult.map CCList.flatten
        in
        (match use_case with
         | Filter.MatchesFilter -> failwith "Invalid use case"
         | Filter.Matcher _ ->
           contacts
           |> Lwt_list.fold_left_s
                (fun (invitations, contacts) contact ->
                   Contact.id contact
                   |> Invitation.find_by_contact_and_experiment_opt
                        pool
                        experiment.Experiment.id
                   |> Lwt.map (function
                     | None -> invitations, contacts @ [ contact ]
                     | Some invitation -> invitations @ [ invitation ], contacts))
                ([], [])
           >|> fun (invitations, contacts) ->
           let* resend_events = resend_existing invitations |> Lwt_result.lift in
           let* create_events = create_new contacts |> Lwt_result.lift in
           Lwt_result.return (create_events @ resend_events)))
    ||> CCList.all_ok
    >|+ CCList.flatten
  in
  match events with
  | Ok events when CCList.is_empty events ->
    Logs.info ~src (fun m -> m ~tags "No action");
    Lwt.return []
  | Ok events -> Lwt.return events
  | Error err ->
    let open Pool_common in
    let (_ : Pool_message.Error.t) = Utils.with_log_error ~tags err in
    Lwt.return []
;;

let smtp_limits_of_id pool =
  let open Email.SmtpAuth in
  let default = RateLimit.default, InvitationCapacity.default in
  let limits { rate_limit; invitation_capacity; _ } = rate_limit, invitation_capacity in
  function
  | None -> find_default_opt pool ||> CCOption.map_or ~default limits
  | Some id ->
    (match%lwt find pool id with
     | Ok smtp when Default.value smtp.default ->
       find_default_opt pool ||> CCOption.map_or ~default limits
     | Ok smtp -> Lwt.return (limits smtp)
     | Error _ -> Lwt.return default)
;;

let compute_limited_mailings pool interval =
  let%lwt status_with_smtp =
    Mailing.Status.find_current pool interval
    >|> Lwt_list.filter_map_s (fun ({ Mailing.Status.mailing; _ } as status) ->
      let find_experiment { Mailing.id; _ } =
        Experiment.find_of_mailing pool (id |> Mailing.Id.to_common)
      in
      let with_spots ({ Experiment.smtp_auth_id; _ } as experiment) =
        match%lwt experiment_has_bookable_spots pool experiment with
        | true -> Lwt.return_ok (status, smtp_auth_id)
        | false -> Lwt.return_error Pool_message.Error.SessionFullyBooked
      in
      mailing
      |> find_experiment
      >>= with_spots
      >|- Pool_common.Utils.with_log_error ~level:Logs.Warning
      ||> CCResult.to_opt)
  in
  (* Normalize: if smtp_auth_id points to the default account, treat it as None
     so that mailings without a custom SMTP and those using the default are grouped together *)
  let%lwt status_with_smtp =
    status_with_smtp
    |> Lwt_list.map_s (fun (status, smtp_auth_id) ->
      match smtp_auth_id with
      | None -> Lwt.return (status, None)
      | Some id ->
        (match%lwt Email.SmtpAuth.find pool id with
         | Ok { Email.SmtpAuth.default; _ } when Email.SmtpAuth.Default.value default ->
           Lwt.return (status, None)
         | Ok _ | Error _ -> Lwt.return (status, smtp_auth_id)))
  in
  let smtp_id_eq = CCOption.equal Email.SmtpAuth.Id.equal in
  let groups =
    CCList.fold_left
      (fun acc (status, smtp_auth_id) ->
         let existing =
           CCList.Assoc.get ~eq:smtp_id_eq smtp_auth_id acc |> CCOption.get_or ~default:[]
         in
         CCList.Assoc.set ~eq:smtp_id_eq smtp_auth_id (existing @ [ status ]) acc)
      []
      status_with_smtp
  in
  groups
  |> Lwt_list.map_s (fun (smtp_auth_id, statuses) ->
    let%lwt rate_limit, invitation_capacity = smtp_limits_of_id pool smtp_auth_id in
    (* Daily cap = rate_limit * invitation_capacity% *)
    let daily_cap =
      let open CCFloat in
      CCInt.to_float (Email.SmtpAuth.RateLimit.value rate_limit)
      *. (CCInt.to_float (Email.SmtpAuth.InvitationCapacity.value invitation_capacity)
          /. 100.)
      |> floor
      |> to_int
    in
    (* Rolling 24h window: count invitations already sent *)
    let%lwt already_sent =
      Email.SmtpAuth.count_invitations_sent_since pool smtp_auth_id 86400
    in
    (* Remaining budget for the rest of today's window *)
    let remaining = max 0 (daily_cap - already_sent) in
    Lwt.return (calculate_mailing_limits remaining statuses))
  ||> CCList.flatten
;;

let create_invitation_events ?invitation_ids interval database_label =
  let%lwt limited_mailings = compute_limited_mailings database_label interval in
  events_of_mailings ?invitation_ids database_label limited_mailings
;;

let match_invitations interval database_label =
  let count_mails =
    CCList.filter_map
      (let open Pool_event in
       function[@warning "-4"]
       | Email (Email.Sent _) -> Some 1
       | Email (Email.BulkSent mails) -> Some (CCList.length mails)
       | _ -> None)
    %> sum
  in
  let%lwt events = create_invitation_events interval database_label in
  Logs.info ~src (fun m ->
    m
      ~tags:(Database.Logger.Tags.create database_label)
      "Sending %4d invitation emails"
      (count_mails events));
  Pool_event.handle_system_events database_label events
;;

let start_matcher () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s (5 * 60) in
  Database.Pool.Tenant.all ()
  |> Lwt_list.iter_s (fun database_label ->
    let label = [%string "matcher [%{Database.Label.value database_label}]"] in
    Logs.debug ~src (fun m ->
      m ~tags:(Database.Logger.Tags.create database_label) "Starting %s" label);
    let schedule =
      create
        label
        (Every (interval |> ScheduledTimeSpan.of_span))
        (Some database_label)
        (fun () -> match_invitations interval database_label)
    in
    Schedule.add_and_start schedule)
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
