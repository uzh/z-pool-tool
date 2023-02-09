let run_forever () = Lwt.wait () |> fst

let run ?(services = []) () =
  Command_utils.make_no_args
    "worker.run"
    "Run worker (no server services)"
    (fun () ->
    Logs.debug (fun m ->
      m
        "Running the following services: %s"
        ([%show: string list]
           (services |> CCList.map Sihl.Container.Service.name)));
    let services =
      CCList.filter CCFun.(Sihl.Container.Service.server %> not) services
    in
    let%lwt (_ : Sihl.Container.lifecycle list) =
      Sihl.Container.start_services services
    in
    run_forever ())
;;
