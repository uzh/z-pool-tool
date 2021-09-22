let handle_conformist_error (err : Conformist.error list) =
  String.concat
    "\n"
    (List.map (fun (m, _, k) -> Format.asprintf "%s: %s" m k) err)
;;

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
        <smtp_auth_server>                  : string
        <smtp_auth_port>                    : '25' | '465' | '587'
        <smtp_auth_username>                : string
        <smtp_auth_authentication_method>   : string
        <smtp_auth_protocol>                : 'STARTTLS' | 'SSL/TLS'
        <styles>                            : string
        <icon>                              : string
        <logos>                             : string
        <partner_logos>                     : string
        <default_language>                  : 'DE' | 'EN'
      |}
      in
      match args with
      | [ title
        ; description
        ; url
        ; database_url
        ; smtp_auth_server
        ; smtp_auth_port
        ; smtp_auth_username
        ; smtp_auth_authentication_method
        ; smtp_auth_protocol
        ; styles
        ; icon
        ; logos
        ; partner_logos
        ; default_language
        ] ->
        let open Lwt.Syntax in
        let* result =
          let open Utils.Lwt_result.Infix in
          let run_command () =
            Lwt_result.lift
            @@
            let open CCResult.Infix in
            Cqrs_command.Tenant_command.AddTenant.decode
              [ "title", [ title ]
              ; "description", [ description ]
              ; "url", [ url ]
              ; "database_url", [ database_url ]
              ; "smtp_auth_server", [ smtp_auth_server ]
              ; "smtp_auth_port", [ smtp_auth_port ]
              ; "smtp_auth_username", [ smtp_auth_username ]
              ; ( "smtp_auth_authentication_method"
                , [ smtp_auth_authentication_method ] )
              ; "smtp_auth_protocol", [ smtp_auth_protocol ]
              ; "styles", [ styles ]
              ; "icon", [ icon ]
              ; "logos", [ logos ]
              ; "partner_logos", [ partner_logos ]
              ; "default_language", [ default_language ]
              ]
            |> CCResult.map_err handle_conformist_error
            >>= Cqrs_command.Tenant_command.AddTenant.handle
          in
          let run_events events =
            let* _ = Lwt_list.map_s Pool_event.handle_event events in
            Lwt.return_ok ()
          in
          () |> run_command >>= run_events
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
