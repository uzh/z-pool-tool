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
         Key.(Hardcoded Name)
         Operator.Equal
         (Single (Str "Bar")))
  in
  let p3 =
    Pred
      (Predicate.create
         Key.(Hardcoded Name)
         Operator.Equal
         (Lst [ Str "Bart"; Str "Homer" ]))
  in
  And (p1, Or (p2, p3))
;;

let filter pool =
  let%lwt experiments = Experiment.find_all pool () in
  let filter_events, experiment_events =
    CCList.fold_left
      (fun (filter_events, experiment_events) experiment ->
        let filter = Filter.create test_filter in
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
