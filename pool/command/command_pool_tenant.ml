let create_tenant_pool =
  let help =
    {|<title> <description> <url> <database_url> <database_label>
      <smtp_auth_server> <smtp_auth_port> <smtp_auth_username>
      <smtp_auth_password> <smtp_auth_authentication_method>
      <smtp_auth_protocol> <styles> <icon> <logos> <default_language>
      <operator_email> <operator_password> <operator_firstname> <operator_lastname>

Provide all fields to create a new tenant:
      <title>                             : string
      <description>                       : string
      <url>                               : string
      <database_url>                      : string
      <database_label>                    : string
      <smtp_auth_server>                  : string
      <smtp_auth_port>                    : string
      <smtp_auth_username>                : string
      <smtp_auth_password>                : string
      <smtp_auth_authentication_method>   : string
      <smtp_auth_protocol>                : 'STARTTLS' | 'SSL/TLS'
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
      ; smtp_auth_server
      ; smtp_auth_port
      ; smtp_auth_username
      ; smtp_auth_password
      ; smtp_auth_authentication_method
      ; smtp_auth_protocol
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
        Cqrs_command.Pool_tenant_command.Create.decode
          [ "title", [ title ]
          ; "description", [ description ]
          ; "url", [ url ]
          ; "database_url", [ database_url ]
          ; "database_label", [ database_label ]
          ; "smtp_auth_server", [ smtp_auth_server ]
          ; "smtp_auth_port", [ smtp_auth_port ]
          ; "smtp_auth_username", [ smtp_auth_username ]
          ; "smtp_auth_password", [ smtp_auth_password ]
          ; ( "smtp_auth_authentication_method"
            , [ smtp_auth_authentication_method ] )
          ; "smtp_auth_protocol", [ smtp_auth_protocol ]
          ; "styles", [ styles ]
          ; "icon", [ icon ]
          ; "logos", [ logos ]
          ; "language", [ default_language ]
          ; "email", [ email ]
          ; "password", [ password ]
          ; "firstname", [ firstname ]
          ; "lastname", [ lastname ]
          ]
        >>= Cqrs_command.Pool_tenant_command.Create.handle
        |> Pool_common.Utils.get_or_failwith
        |> Lwt_list.iter_s (Pool_event.handle_event Pool_database.root)
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
        let open Cqrs_command.Pool_tenant_command.EditDatabase in
        let* tenant = find_by_label pool >>= fun { id; _ } -> find_full id in
        let* url = Database.Url.create database_url |> Lwt_result.lift in
        let updated_database =
          Database.
            { database_url = url; database_label = tenant.Write.database.label }
        in
        handle tenant updated_database |> Lwt.return
      in
      (match%lwt result with
       | Ok events ->
         let%lwt () = Pool_event.handle_events Pool_database.root events in
         Lwt.return_some ()
       | Error err ->
         let open Pool_common in
         let (_ : Message.error) = Utils.with_log_error err in
         Lwt.return_none)
    | _ -> Command_utils.failwith_missmatch help)
;;
