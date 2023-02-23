let run_forever () = Lwt.wait () |> fst

let run ?(services = []) () =
  let dependencies =
    CCList.map
      (fun { Sihl.Container.Service.lifecycle; _ } -> lifecycle)
      services
  in
  let name = "worker" in
  let description = "Run worker (no server services)" in
  let help = Format.asprintf {|

Example: %s
  |} name in
  Sihl.Command.make ~name ~description ~help ~dependencies (function
    | [] -> run_forever ()
    | _ -> Command_utils.failwith_missmatch help)
;;
