let halfhour, hour = CCPair.map_same Ptime.Span.of_int_s (30 * 60, 60 * 60)

let session_data =
  [ ( Ptime.add_span (Ptime_clock.now ()) hour
      |> CCOption.get_exn_or "Invalid time"
    , hour
    , None
    , 30
    , 4
    , 4
    , None
    , None
    , Some 3600 )
  ; ( Ptime.add_span (Ptime_clock.now ()) halfhour
      |> CCOption.get_exn_or "Invalid time"
    , halfhour
    , Some "No metal allowed!"
    , 28
    , 20
    , 0
    , None
    , None
    , None )
  ; ( Ptime.add_span (Ptime_clock.now ()) hour
      |> CCOption.get_exn_or "Invalid time"
    , halfhour
    , Some "No metal allowed!"
    , 30
    , 2
    , 5
    , None
    , None
    , Some 7200 )
  ]
;;

let create pool =
  let open CCFun in
  let open Pool_common.Utils in
  let%lwt experiments = Experiment.find_all pool () in
  let%lwt locations = Pool_location.find_all pool in
  let%lwt () =
    Lwt_list.iter_s
      (fun experiment ->
        let main_session_events =
          CCList.map
            (fun ( start
                 , duration
                 , description
                 , max
                 , min
                 , overbook
                 , reminder_subject
                 , reminder_text
                 , reminder_lead_time ) ->
              let open CCOption in
              let (session : Session.base) =
                Session.
                  { start = Start.create start
                  ; duration = Duration.create duration |> get_or_failwith
                  ; description =
                      description >>= Description.create %> of_result
                  ; max_participants =
                      ParticipantAmount.create max |> get_or_failwith
                  ; min_participants =
                      ParticipantAmount.create min |> get_or_failwith
                  ; overbook =
                      ParticipantAmount.create overbook |> get_or_failwith
                  ; reminder_subject
                  ; reminder_text =
                      reminder_text
                      >|= Pool_common.Reminder.Text.create %> get_or_failwith
                  ; reminder_lead_time =
                      reminder_lead_time
                      >|= Ptime.Span.of_int_s
                          %> Pool_common.Reminder.LeadTime.create
                          %> get_or_failwith
                  }
              in
              let location = CCList.hd locations in
              Session.Created (session, None, experiment.Experiment.id, location))
            session_data
        in
        Lwt_list.iter_s (Session.handle_event pool) main_session_events)
      experiments
  in
  Lwt_list.iter_s
    (fun experiment ->
      let%lwt sessions =
        Session.find_all_for_experiment pool experiment.Experiment.id
        |> Lwt.map CCResult.get_exn
      in
      let parent = CCList.hd sessions in
      let (follow_up : Session.base) =
        let open CCOption in
        Session.
          { start =
              Start.create
                (Ptime.add_span (parent.start |> Session.Start.value) hour
                |> CCOption.get_exn_or "Invalid time")
          ; duration = Duration.create halfhour |> get_or_failwith
          ; description = Some "MRI Study" >>= Description.create %> of_result
          ; max_participants = ParticipantAmount.create 10 |> get_or_failwith
          ; min_participants = ParticipantAmount.create 2 |> get_or_failwith
          ; overbook = ParticipantAmount.create 3 |> get_or_failwith
          ; reminder_lead_time = None
          ; reminder_subject = None
          ; reminder_text = None
          }
      in
      let location = CCList.hd locations in
      Session.handle_event pool
      @@ Session.Created
           ( follow_up
           , Some parent.Session.id
           , experiment.Experiment.id
           , location ))
    experiments
;;

(* TODO maybe private constructor needed *)
type single
type multi

type _ val' =
  | Str : string -> single val'
  | Nr : float -> single val'
  | Bool : bool -> single val'
  | Date : Ptime.t -> single val'
  | Empty : single val'
  (* TODO maybe problematic when pattern matching (constructor would escape its
     scope *)
  | Lst : 'a list -> multi val'

type key = string

type _ operator =
  | LT : single operator
  | LE : single operator
  | GT : single operator
  | GE : single operator
  | EQ : single operator
  (* TODO maybe redundant cause have Not and EQ? *)
  | NEQ : single operator
  | SOME : multi operator
  | NONE : multi operator
  | ALL : multi operator

type 'a predicate = key * 'a operator * 'a val'

(* TODO turn into infix constructors *)
type filter =
  | And of filter * filter
  | Or of filter * filter
  | Not of filter
  | PredS of single predicate
  | PredM of multi predicate

let ( &.& ) a b = And (a, b)
let ( |.| ) a b = Or (a, b)

(* TODO make this prefix *)
let ( --. ) a = Not a

(* role: only participant is shown by default
 * unverified email: only users with confirmed email are shown by default
 * paused: hidden by default
 * deactivated: hidden by default
 * tags: empty by default, depends on #23 *)

let foo : filter =
  PredS ("role", EQ, Str "participant") &.& Not (PredS ("verified", EQ, Empty))
;;

let bla : filter = PredS ("paused", EQ, Bool false)
let bar : filter = PredM ("tags", SOME, Lst [ "pregnant" ])
let baz : filter = PredM ("experiment_id", NONE, Lst [ 2, 3 ])
