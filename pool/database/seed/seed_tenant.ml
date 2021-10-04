let create () =
  let open Lwt.Syntax in
  let data =
    [ ( "Econ uzh"
      , "description"
      , "pool.econ.uzh.ch"
      , "mariadb://root@database:3306/dev_econ"
      , "econ-uzh"
      , "smtp.uzh.ch"
      , "587"
      , "engineering@econ.uzh.ch"
      , "emailemail"
      , "LOGIN"
      , "STARTTLS"
      , "custom-styles.econ.css"
      , "some icon"
      , "some logo"
      , "some partner logos"
      , "EN"
      , "operator@econ.uzh.ch"
      , "adminadmin"
      , "DJ"
      , "Ötzi" )
    ; ( "ZHAW"
      , "description"
      , "pool.zhaw.ch"
      , "database@mariadb://root@database:3306/dev_zhaw"
      , "zhaw"
      , "smtp.zhaw.ch"
      , "465"
      , "engineering@zhaw.ch"
      , "emailemail"
      , "LOGIN"
      , "SSL/TLS"
      , "custom-styles.zhaw.css"
      , "some icon"
      , "some logo"
      , "some partner logos"
      , "DE"
      , "operator@zhaw.ch"
      , "adminadmin"
      , "Woofy"
      , "Woofer" )
    ]
  in
  let* _ =
    Lwt_list.map_s
      (fun ( title
           , description
           , url
           , database_url
           , database_label
           , smtp_auth_server
           , smtp_auth_port
           , smtp_auth_username
           , smtp_auth_password
           , smtp_auth_authentication_method
           , smtp_auth_protocol
           , styles
           , icon
           , logos
           , partner_logos
           , default_language
           , email
           , password
           , firstname
           , lastname ) ->
        let open Lwt.Syntax in
        let* result =
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
            >>= Cqrs_command.Tenant_command.Create.handle
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
