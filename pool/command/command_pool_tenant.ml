let failwith = Pool_common.Utils.get_or_failwith
let src = Logs.Src.create "command.pool_tenant"

let create_tenant_pool =
  let help =
    {|<title> <description> <url> <database_url> <database_label>
      <styles> <icon> <logos> <default_language> <operator_email>
      <operator_password> <operator_firstname> <operator_lastname>

Provide all fields to create a new tenant:
      <title>                             : string
      <description>                       : string
      <url>                               : string
      <database_url>                      : string
      <database_label>                    : string
      <styles>                            : uuid
      <icon>                              : uuid
      <logos>                             : uuid
      <default_language>                  : 'DE' | 'EN'
      <operator_email>                    : string
      <operator_password>                 : string
      <operator_firstname>                : string
      <operator_lastname>                 : string
  |}
  in
  Sihl.Command.make
    ~name:"tenant.create"
    ~description:"Creates a new test tenant"
    ~help
    (function
    | [ title
      ; description
      ; url
      ; database_url
      ; database_label
      ; styles
      ; icon
      ; logos
      ; default_language
      ; email
      ; password
      ; firstname
      ; lastname
      ] ->
      let%lwt () =
        let open CCResult.Infix in
        let%lwt database =
          Database.Pool.create_validated_and_tested database_label database_url
          |> Lwt.map failwith
        in
        Cqrs_command.Pool_tenant_command.Create.decode
          [ "title", [ title ]
          ; "description", [ description ]
          ; "url", [ url ]
          ; "styles", [ styles ]
          ; "icon", [ icon ]
          ; "logos", [ logos ]
          ; "language", [ default_language ]
          ; "email", [ email ]
          ; "password", [ password ]
          ; "firstname", [ firstname ]
          ; "lastname", [ lastname ]
          ]
        >>= Cqrs_command.Pool_tenant_command.Create.handle database
        |> Pool_common.Utils.get_or_failwith
        |> Lwt_list.iter_s (Pool_event.handle_event Database.Pool.Root.label)
      in
      Lwt.return_some ()
    | _ -> Command_utils.failwith_missmatch help)
;;

let update_tenant_database_url =
  let name = "tenant.database.update_url" in
  let description = "Update the database url of the specified tenant." in
  let help =
    Format.asprintf
      {|<database_label> <database_url>

Provide the following variables:
        <database_label>      : string
        <database_url>        : string


Example: %s econ-uzh mariadb://user:pw@localhost:3306/dev_econ
  |}
      name
  in
  Sihl.Command.make ~name ~description ~help (function
    | [ pool; database_url ] ->
      let open Utils.Lwt_result.Infix in
      let open Pool_tenant in
      let%lwt pool = Command_utils.is_available_exn pool in
      let result =
        let open Cqrs_command.Pool_tenant_command.UpdateDatabase in
        let* tenant = find_by_label pool >>= fun { id; _ } -> find_full id in
        let%lwt updated_database =
          Database.Pool.create_validated_and_tested
            (tenant |> Write.database_label |> Database.Label.value)
            database_url
          |> Lwt.map failwith
        in
        handle tenant updated_database |> Lwt.return
      in
      (match%lwt result with
       | Ok events ->
         let%lwt () =
           Pool_event.handle_events Database.Pool.Root.label events
         in
         Lwt.return_some ()
       | Error err ->
         let open Pool_common in
         let (_ : Pool_message.Error.t) =
           Utils.with_log_error
             ~src
             ~tags:(Database.Logger.Tags.create pool)
             err
         in
         Lwt.return_none)
    | _ -> Command_utils.failwith_missmatch help)
;;
