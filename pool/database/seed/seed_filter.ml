let test_filter =
  let open Filter in
  let open Operator in
  let p1 =
    Pred
      (Predicate.create
         Key.(Hardcoded ContactLanguage)
         (Equality.Equal |> equality)
         (Single (Language Pool_common.Language.En)))
  in
  let p2 =
    Pred
      (Predicate.create
         Key.(Hardcoded NumAssignments)
         (Size.Less |> size)
         (Single (Nr 5.)))
  in
  Or [ p1; p2 ]
;;

let filter pool =
  let%lwt experiments, (_ : Query.t) = Experiment.find_all pool in
  let filter_events, experiment_events =
    CCList.fold_left
      (fun (filter_events, experiment_events) experiment ->
        let open Experiment in
        let filter = Filter.create None test_filter in
        ( Filter.Created filter :: filter_events
        , Updated { experiment with filter = Some filter } :: experiment_events
        ))
      ([], [])
      experiments
  in
  let%lwt () = Lwt_list.iter_s (Filter.handle_event pool) filter_events in
  let%lwt () =
    Lwt_list.iter_s (Experiment.handle_event pool) experiment_events
  in
  Lwt.return_unit
;;
