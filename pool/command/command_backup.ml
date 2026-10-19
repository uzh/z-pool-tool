open CCFun

let max_allowed_packet_flag = "--max-allowed-packet="

let create =
  let name = "db.backup" in
  let description =
    "Dump the root database and all tenant databases registered in it (or a single one) \
     into DATABASE_BACKUP_DIR (default: <ROOT_PATH>/backups). Run it before updating."
  in
  let help =
    Format.asprintf
      {|[%s<size>] [<database_label>]

Provide the following optional variables:
        %s<size>      : maximum length of an INSERT statement, e.g. 16M (default: %i bytes)
        <database_label>      : string, only dump this database (root or a tenant)

Example: %s
         %s econ-uzh
         %s %s16M econ-uzh
  |}
      max_allowed_packet_flag
      max_allowed_packet_flag
      Database_backup.Dump.default_max_allowed_packet
      name
      name
      max_allowed_packet_flag
      name
  in
  let run ?max_allowed_packet label =
    let open Database_backup in
    let%lwt () = Database.Pool.Root.setup () in
    let%lwt registered = Database.Pool.registered () in
    let databases =
      match label with
      | None -> registered
      | Some label ->
        (match
           CCList.filter
             (Database.label %> Database.Label.value %> CCString.equal label)
             registered
         with
         | [] ->
           failwith
             (Format.asprintf
                "The database %s is not registered (%s)."
                label
                (registered
                 |> CCList.map (Database.label %> Database.Label.value)
                 |> CCString.concat ", "))
         | databases -> databases)
    in
    let%lwt { directory; failed } =
      create ?max_allowed_packet ~version:Version.to_string databases
    in
    match failed with
    | [] ->
      print_endline (Format.asprintf "Database backup written to %s" directory);
      Lwt.return_some ()
    | failed ->
      failed
      |> CCList.iter (fun (label, err) ->
        prerr_endline (Format.asprintf "Backup of '%s' failed: %s" label err));
      exit 1
  in
  let max_allowed_packet flag =
    CCString.chop_prefix ~pre:max_allowed_packet_flag flag
    |> CCOption.map (fun size ->
      match Database_backup.parse_size size with
      | Some size -> size
      | None -> Command_utils.failwith_missmatch help)
  in
  Sihl.Command.make ~name ~description ~help (function
    | [] -> run None
    | [ argument ] ->
      (match max_allowed_packet argument with
       | Some max_allowed_packet -> run ~max_allowed_packet None
       | None -> run (Some argument))
    | [ flag; label ] ->
      (match max_allowed_packet flag with
       | Some max_allowed_packet -> run ~max_allowed_packet (Some label)
       | None -> Command_utils.failwith_missmatch help)
    | _ -> Command_utils.failwith_missmatch help)
;;

let cleanup =
  Command_utils.make_no_args
    "db.backup.cleanup"
    "Remove all database dumps created by db.backup."
    (fun () ->
       let directory = Database_backup.directory () in
       let removed = Database_backup.remove_all ~directory () in
       print_endline
         (Format.asprintf "Removed %i database backup(s) from %s" removed directory);
       Lwt.return_some ())
;;
