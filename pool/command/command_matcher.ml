let get_or_failwith = Pool_common.Utils.get_or_failwith
let return = Lwt.return_some ()

let run_tenant =
  Sihl.Command.make
    ~name:"matcher.run"
    ~description:"Run invitation matcher"
    ~help:"<Pool_database>"
    (let help_text =
       {|Provide all fields to run the matcher to send experiment invitations:
    <Pool_database>       : string

Example: sihl matcher.run econ-uzh
          |}
     in
     function
     | [ db_pool ] ->
       let () = Database.Root.setup () in
       let%lwt (_ : Pool_database.Label.t list) = Database.Tenant.setup () in
       let pool =
         Pool_database.Label.create db_pool
         |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
         |> CCResult.get_or_failwith
       in
       let%lwt () = Matcher.match_invitations [ pool ] in
       return
     | _ ->
       print_endline help_text;
       failwith "Argument missmatch")
;;

let run_all =
  Sihl.Command.make
    ~name:"matcher.run_all"
    ~description:"Run invitation matcher on all tenants"
    (let open Utils.Lwt_result.Infix in
    let help_text =
      {|No additional arguments allowed.

Example: sihl matcher.run_all
          |}
    in
    function
    | [] ->
      let () = Database.Root.setup () in
      let%lwt () = Database.Tenant.setup () >|> Matcher.match_invitations in
      return
    | _ ->
      print_endline help_text;
      failwith "Argument missmatch")
;;
