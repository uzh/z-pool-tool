let get_or_failwith = Pool_common.Utils.get_or_failwith

let run =
  Sihl.Command.make
    ~name:"matcher.run"
    ~description:"Run invitation matcher"
    ~help:"<Pool_database>"
    (fun args ->
    let open Utils.Lwt_result.Infix in
    let return = Lwt.return_some () in
    let help_text =
      {|Provide all fields to run the matcher to send experiment invitations:
    <Pool_database>       : string

Example: sihl matcher.run econ-uzh
          |}
    in
    match args with
    | [ db_pool ] ->
      let () = Database.Root.setup () in
      let%lwt (_ : Pool_database.Label.t list) = Database.Tenant.setup () in
      let interval = 5 in
      let pool =
        Pool_database.Label.create db_pool
        |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
        |> CCResult.get_or_failwith
      in
      let%lwt events =
        let open Cqrs_command.Matcher_command.Run in
        Mailing.find_current pool
        >|> Lwt_list.map_s (fun mailing ->
              let%lwt experiment, contacts, skip_contacts, i18n_templates =
                Matcher.find_contacts_by_mailing pool ~interval mailing
              in
              { mailing; experiment; contacts; skip_contacts; i18n_templates }
              |> Lwt.return)
        ||> handle
      in
      let%lwt () = events |> get_or_failwith |> Pool_event.handle_events pool in
      return
    | _ ->
      print_endline help_text;
      return)
;;
