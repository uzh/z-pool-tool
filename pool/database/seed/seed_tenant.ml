let create () =
  let open Lwt.Syntax in
  let data =
    [ ( "Econ uzh"
      , "description"
      , "pool.econ.uzh.ch"
      , "database@econ.ch"
      , "smtp.uzh.ch"
      , "587"
      , "engineering@econ.uzh.ch"
      , "LOGIN"
      , "STARTTLS"
      , "custom-styles.econ.css"
      , "some icon"
      , "some logo"
      , "some partner logos"
      , "EN" )
    ; ( "ZHAW"
      , "description"
      , "pool.zhaw.ch"
      , "database@zhaw.ch"
      , "smtp.zhaw.ch"
      , "465"
      , "engineering@zhaw.ch"
      , "LOGIN"
      , "SSL/TLS"
      , "custom-styles.zhaw.css"
      , "some icon"
      , "some logo"
      , "some partner logos"
      , "DE" )
    ]
  in
  let* _ =
    Lwt_list.map_s
      (fun ( title
           , description
           , url
           , database_url
           , smtp_auth_server
           , smtp_auth_port
           , smtp_auth_username
           , smtp_auth_authentication_method
           , smtp_auth_protocol
           , styles
           , icon
           , logos
           , partner_logos
           , default_language ) ->
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
            |> CCResult.map_err Utils.handle_conformist_error
            >>= Cqrs_command.Tenant_command.AddTenant.handle
          in
          let run_events events =
            let* _ = Lwt_list.map_s Pool_event.handle_event events in
            Lwt.return_ok ()
          in
          () |> run_command >>= run_events
        in
        match result with
        | Ok _ -> Lwt.return_ok ()
        | Error err ->
          print_endline err;
          Lwt.return_error err)
      data
  in
  Lwt.return_unit
;;
