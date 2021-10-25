let create_tenant =
  Sihl.Command.make
    ~name:"tenant.create"
    ~help:
      "<title> <description> <url> <database_url> <smtp_auth_server> \
       <smtp_auth_port> <smtp_auth_username> <smtp_auth_authentication_method> \
       <smtp_auth_protocol> <styles> <icon> <logos> <partner_logos> \
       <default_language>"
    ~description:"Creates a new test tenant"
    (fun args ->
      let help_text =
        {|Provide all fields to create a new tenant:
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
        <smtp_auth_protocol>                : 'STARTTLS' | 'SSL/TLS'
        <styles>                            : string
        <icon>                              : string
        <logos>                             : string
        <partner_logos>                     : string
        <default_language>                  : 'DE' | 'EN'
        <operator_email>                    : string
        <operator_password>                 : string
        <operator_firstname>                : string
        <operator_lastname>                 : string
      |}
      in
      match args with
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
        ; partner_logos
        ; default_language
        ; email
        ; password
        ; firstname
        ; lastname
        ] ->
        let%lwt result =
          let open Utils.Lwt_result.Infix in
          let run_command () =
            Lwt_result.lift
            @@
            let open CCResult.Infix in
            Cqrs_command.Tenant_command.Create.decode
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
              ; "partner_logos", [ partner_logos ]
              ; "default_language", [ default_language ]
              ; "email", [ email ]
              ; "password", [ password ]
              ; "firstname", [ firstname ]
              ; "lastname", [ lastname ]
              ]
            |> CCResult.map_err Utils.handle_conformist_error
            (* TODO [timhub]: How to deal with seeds and files? *)
            >>= Cqrs_command.Tenant_command.Create.handle []
          in
          let run_events =
            Lwt_list.iter_s (Pool_event.handle_event Pool_common.Database.root)
          in
          () |> run_command |>> run_events
        in
        (match result with
        | Ok _ -> Lwt.return_some ()
        | Error err ->
          print_endline err;
          Lwt.return_some ())
      | _ ->
        print_endline help_text;
        Lwt.return_some ())
;;
