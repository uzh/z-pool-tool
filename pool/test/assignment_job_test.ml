open Test_utils
open Integration_utils
open Utils.Lwt_result.Infix
open Assignment_job

let pool = Data.database_label
let get_exn = get_or_failwith
let contact_id_1 = Contact.Id.create ()
let contact_id_2 = Contact.Id.create ()
let name_1 = Pool_user.Lastname.of_string "foo"
let name_2 = Pool_user.Lastname.of_string "bar"
let get_contact id = Contact.find pool id ||> get_exn
let contacts () = Lwt.both (get_contact contact_id_1) (get_contact contact_id_2)
let experiment_id = Experiment.Id.create ()
let session_id = Session.Id.create ()
let get_experiment () = Experiment.find pool experiment_id ||> get_exn
let get_session () = Session.find pool session_id ||> get_exn
let admin = Model.create_admin () |> Pool_context.get_admin_user |> get_exn

let get_assignment_by_contact contact_id =
  Assignment.find_all_by_session pool session_id
  ||> CCList.find (fun { Assignment.contact; _ } ->
    let open Contact in
    Id.equal (id contact) contact_id)
;;

let create_notification experiment assignments =
  let%lwt tenant = Pool_tenant.find_by_label pool ||> get_exn in
  Message_template.MatchFilterUpdateNotification.create
    tenant
    ~text:
      Pool_common.(
        Utils.text_to_string Language.En I18n.MatchesFilterChangeReasonWorker)
    admin
    experiment
    assignments
;;

let init _ () =
  let open ContactRepo in
  let%lwt c1 = create ~id:contact_id_1 ~lastname:name_1 () in
  let%lwt c2 = create ~id:contact_id_2 ~lastname:name_2 () in
  let%lwt experiment = ExperimentRepo.create ~id:experiment_id () in
  let%lwt session = SessionRepo.create ~id:session_id experiment () in
  let sign_up = AssignmentRepo.create session in
  let%lwt (_ : Assignment.t list) = [ c1; c2 ] |> Lwt_list.map_s sign_up in
  Lwt.return_unit
;;

let to_events (assignment_events, emails) =
  let assignments = CCList.map Pool_event.assignment assignment_events in
  match emails with
  | [] -> assignments
  | emails -> assignments @ [ Pool_event.email (Email.BulkSent emails) ]
;;

let update_without_filter _ () =
  let%lwt session = get_session () in
  let%lwt events =
    update_matches_filter pool (`Session session) >|+ to_events
  in
  check_result (Ok []) events |> Lwt.return
;;

let exclude_contact _ () =
  let%lwt current_user =
    Integration_utils.AdminRepo.create () ||> Pool_context.admin
  in
  let%lwt assignment = get_assignment_by_contact contact_id_1 in
  let exclude_1 =
    let open Filter in
    let predicate : Predicate.t =
      let key : Key.t = Key.(Hardcoded Name) in
      let operator = Operator.(Equality Equality.NotEqual) in
      let value = Single (Str (Pool_user.Lastname.value name_1)) in
      Predicate.{ key; operator; value }
    in
    create None (Pred predicate)
  in
  let%lwt () =
    let%lwt experiment = get_experiment () in
    Filter_test.save_filter current_user exclude_1 experiment
  in
  let%lwt experiment = get_experiment () in
  let%lwt session = get_session () in
  let%lwt expected =
    let%lwt message =
      create_notification experiment [ session, [ assignment ] ]
    in
    [ Assignment.(
        Updated { assignment with matches_filter = MatchesFilter.create false })
      |> Pool_event.assignment
    ; Email.BulkSent [ message ] |> Pool_event.email
    ]
    |> Lwt_result.return
  in
  let%lwt events =
    update_matches_filter ~current_user:admin pool (`Session session)
    >|+ to_events
  in
  check_result expected events |> Lwt.return
;;
