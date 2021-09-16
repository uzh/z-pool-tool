let create_tenant =
  Sihl.Command.make
    ~name:"tenant.create"
    ~help:"<title> <description> <url> <database>"
    ~description:"Creates a new test tenant"
    (fun args ->
      let open CCResult.Infix in
      let ( let* ) = Result.bind in
      match args with
      | title :: description :: url :: database :: tl when List.length tl == 0
        ->
        let create () =
          let open Tenant in
          let* title = Title.create title in
          let* description = Description.create description in
          let* url = Url.create url in
          let* database = Database.create database in
          let* smtp_auth_server = SmtpAuth.Server.create "smtp.uzh.ch" in
          let* smtp_auth_port = SmtpAuth.Port.create "587" in
          let* smtp_auth_username =
            SmtpAuth.Username.create "engineering@econ.uzh.ch"
          in
          let* smtp_auth_authentication_method =
            SmtpAuth.AuthenticationMethod.create "LOGIN"
          in
          let* smtp_auth_protocol = SmtpAuth.Protocol.create "SSL/TLS" in
          let* styles = Styles.create "custom_stylesheet.css" in
          let* icon = Icon.create "some icon" in
          let* logos = Logos.create "some logos" in
          let* partner_logos = PartnerLogo.create "some partner" in
          let* default_language = Settings.Language.of_string "EN" in
          let t : Cqrs_command.Tenant_command.AddTenant.t =
            { title
            ; description
            ; url
            ; database
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
            }
          in
          Ok t
        in
        let handle (tenant : Cqrs_command.Tenant_command.AddTenant.t) =
          let* events = Cqrs_command.Tenant_command.AddTenant.handle tenant in
          let _ =
            CCList.map (fun event -> Pool_event.handle_event event) events
          in
          Result.ok ()
        in
        let log res =
          match res with
          | Ok _ ->
            Logs.info (fun m -> m "Tenant successfully created");
            Lwt.return_some ()
          | Error msg ->
            Logs.err (fun m -> m "%s" msg);
            Lwt.return_none
        in
        () |> create >|= handle |> log
      | _ ->
        Logs.err (fun m -> m "Invalid command for generating payout!");
        Lwt.return_none)
;;
