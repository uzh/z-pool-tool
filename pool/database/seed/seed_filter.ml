let test_filter =
  let open Filter in
  let p1 =
    Pred
      (Predicate.create
         Key.(Hardcoded Email)
         Operator.Equal
         (Single (Str "Foo")))
  in
  let p2 =
    Pred
      (Predicate.create
         Key.(Hardcoded Email)
         Operator.Like
         (Single (Str "%email.com")))
  in
  Or [ p1; p2 ]
;;

let filter pool =
  let%lwt experiments = Experiment.find_all pool () in
  let filter_events, experiment_events =
    CCList.fold_left
      (fun (filter_events, experiment_events) experiment ->
        let filter = Filter.create None test_filter in
        ( Filter.Created filter :: filter_events
        , Experiment.(Updated { experiment with filter = Some filter })
          :: experiment_events ))
      ([], [])
      experiments
  in
  let%lwt () = Lwt_list.iter_s (Filter.handle_event pool) filter_events in
  let%lwt () =
    Lwt_list.iter_s (Experiment.handle_event pool) experiment_events
  in
  Lwt.return_unit
;;
