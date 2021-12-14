module Assets = Seed_assets
module File = Pool_common.File

let print_error = function
  | Ok _ -> Lwt.return_unit
  | Error err ->
    print_endline err;
    Lwt.return_unit
;;

let create () =
  let styles = Assets.dummy_css () in
  let icon = Assets.dummy_icon () in
  let tenant_logo = Assets.dummy_tenant_logo () in
  let partner_logo = Assets.dummy_partner_logo () in
  let%lwt () =
    Lwt_list.iter_s
      (fun file ->
        let open Assets in
        let stored_file =
          Sihl_storage.
            { id = file.Assets.id
            ; filename = file.filename
            ; filesize = file.filesize
            ; mime = file.mime
            }
        in
        let base64 = Base64.encode_exn file.body in
        let%lwt _ = Service.Storage.upload_base64 stored_file base64 in
        Lwt.return_unit)
      [ styles; icon; tenant_logo; partner_logo ]
  in
  let logos =
    let open Tenant_pool.LogoMapping.LogoType in
    [ to_string TenantLogo, [ tenant_logo.Assets.id ]
    ; to_string PartnerLogo, [ partner_logo.Assets.id ]
    ]
  in
  let data =
    if Sihl.Configuration.is_test ()
    then (
      let password =
        Sihl.Configuration.read_string "MYSQL_ROOT_PASSWORD"
        |> CCOption.get_exn_or "MYSQL_ROOT_PASSWORD undefined"
      in
      let database =
        Sihl.Configuration.read_string "MYSQL_DATABASE"
        |> CCOption.get_exn_or "MYSQL_DATABASE undefined"
      in
      [ ( "Econ test"
        , "description"
        , "test.pool.econ.uzh.ch"
        , Format.asprintf
            "mariadb://root:%s@database-tenant:3306/%s"
            password
            database
        , "econ-test"
        , "smtp.uzh.ch"
        , "587"
        , "test@econ.uzh.ch"
        , "emailemail"
        , "LOGIN"
        , "STARTTLS"
        , styles.Assets.id
        , icon.Assets.id
        , "EN"
        , "operator@econ.uzh.ch"
        , "adminadmin"
        , "Test"
        , "Operator" )
      ])
    else
      [ ( "Econ uzh"
        , "description"
        , "localhost:3017"
        , "mariadb://root@database-tenant:3306/dev_econ"
        , "econ-uzh"
        , "smtp.uzh.ch"
        , "587"
        , "engineering@econ.uzh.ch"
        , "emailemail"
        , "LOGIN"
        , "STARTTLS"
        , styles.Assets.id
        , icon.Assets.id
        , "EN"
        , "operator@econ.uzh.ch"
        , "adminadmin"
        , "DJ"
        , "Ötzi" )
      ; ( "ZHAW"
        , "description"
        , "pool.zhaw.ch"
        , "mariadb://root@database-tenant:3306/dev_zhaw"
        , "zhaw"
        , "smtp.zhaw.ch"
        , "465"
        , "engineering@zhaw.ch"
        , "emailemail"
        , "LOGIN"
        , "SSL/TLS"
        , styles.Assets.id
        , icon.Assets.id
        , "DE"
        , "operator@zhaw.ch"
        , "adminadmin"
        , "Woofy"
        , "Woofer" )
      ]
  in
  let%lwt () =
    Lwt_list.iter_s
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
           , default_language
           , email
           , password
           , firstname
           , lastname ) ->
        let open CCResult.Infix in
        let open Cqrs_command.Tenant_pool_command.Create in
        logos
        @ [ "title", [ title ]
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
          ; "default_language", [ default_language ]
          ; "email", [ email ]
          ; "password", [ password ]
          ; "firstname", [ firstname ]
          ; "lastname", [ lastname ]
          ]
        |> decode
        >>= handle
        |> function
        | Ok events ->
          Lwt_list.iter_s (Pool_event.handle_event Database_pool.root) events
        | Error err ->
          failwith Pool_common.(Utils.error_to_string Language.En err))
      data
  in
  Lwt.return_unit
;;
