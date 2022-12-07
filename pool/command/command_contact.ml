let sign_up =
  Sihl.Command.make
    ~name:"contact.signup"
    ~description:"New contact signup"
    ~help:
      "<Pool_database> <email> <password> <firstname> <lastname> \
       <recruitment_channel> <terms_excepted>"
    (fun args ->
    let return = Lwt.return_some () in
    let help_text =
      {|Provide all fields to sign up a new contact:
    <Pool_database>       : string
    <email>               : string
    <password>            : string
    <firstname>           : string
    <lastname>            : string
    <recruitment_channel> : string of 'friend', 'online', 'lecture', 'mailing'
    <language>            : string of 'DE', 'EN'
    <terms_accepted>      : string 'accept' everything else is treated as declined

Example: sihl contact.signup econ-uzh example@mail.com securePassword Max Muster online

Note: Make sure 'accept' is added as final argument, otherwise signup fails.
          |}
    in
    match args with
    | [ db_pool
      ; email
      ; password
      ; firstname
      ; lastname
      ; recruitment_channel
      ; language
      ; terms_accepted
      ]
      when CCString.equal terms_accepted "accept" ->
      let db_pool =
        Pool_database.Label.create db_pool
        |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
        |> CCResult.get_or_failwith
      in
      Database.Root.setup ();
      let%lwt available_pools = Database.Tenant.setup () in
      if CCList.mem ~eq:Pool_database.Label.equal db_pool available_pools
      then (
        let%lwt events =
          let open CCResult.Infix in
          let open Lwt_result.Syntax in
          let open Cqrs_command.Contact_command.SignUp in
          let* tenant = Pool_tenant.find_by_label db_pool in
          let language =
            Pool_common.Language.create language |> CCResult.to_opt
          in
          [ "email", [ email ]
          ; "password", [ password ]
          ; "firstname", [ firstname ]
          ; "lastname", [ lastname ]
          ; "recruitment_channel", [ recruitment_channel ]
          ]
          |> decode
          >>= handle tenant language
          |> Lwt_result.lift
        in
        match events with
        | Error err -> failwith (Pool_common.Message.show_error err)
        | Ok events ->
          let%lwt handle_event =
            Lwt_list.iter_s (Pool_event.handle_event db_pool) events
          in
          Lwt.return_some handle_event)
      else (
        print_endline "The specified database pool is not available.";
        return)
    | _ ->
      print_endline help_text;
      return)
;;

let create_message contact template =
  (* TODO[tinhub]: Sihl 4.0: add text elements to for subject *)
  let name = Contact.fullname contact in
  let email = Contact.email_address contact in
  Email.Helper.prepare_boilerplate_email
    template
    (email |> Pool_user.EmailAddress.value)
    [ "name", name ]
;;

let trigger_profile_update_by_tenant pool =
  let open Utils.Lwt_result.Infix in
  let* tenant = Pool_tenant.find_by_label pool in
  let* contacts = Contact.find_to_trigger_profile_update pool in
  match contacts with
  | [] -> Lwt_result.return ()
  | contacts ->
    let* default_language = Settings.default_language pool in
    let template (msg_language : Pool_common.Language.t) =
      let i18n_texts =
        Hashtbl.create ~random:true (CCList.length Pool_common.Language.all)
      in
      match Hashtbl.find_opt i18n_texts msg_language with
      | Some template -> Lwt.return_ok template
      | None ->
        let* subject =
          I18n.(find_by_key pool Key.TriggerProfileUpdateSubject msg_language)
          >|+ Email.CustomTemplate.Subject.i18n
        in
        let* content =
          I18n.(find_by_key pool Key.TriggerProfileUpdateText msg_language)
          >|+ Email.CustomTemplate.Content.i18n
        in
        let layout = Email.Helper.layout_from_tenant tenant in
        let template = Email.CustomTemplate.{ subject; content; layout } in
        let () = Hashtbl.add i18n_texts msg_language template in
        Lwt_result.return template
    in
    let messages =
      Lwt_list.map_s
        (fun (contact : Contact.t) ->
          let* template =
            template
              (CCOption.value
                 ~default:default_language
                 contact.Contact.language)
          in
          create_message contact template |> Lwt_result.ok)
        contacts
      ||> CCResult.flatten_l
    in
    messages
    >>= fun emails ->
    Cqrs_command.Contact_command.SendProfileUpdateTrigger.(
      { contacts; emails }
      |> handle
      |> Lwt_result.lift
      |>> Pool_event.handle_events pool)
;;

let tenant_specific_profile_update_trigger =
  Sihl.Command.make
    ~name:"trigger_profile_update.send"
    ~description:"Send profile update triggers of all tenants"
    (fun args ->
    match args with
    | [ pool ] ->
      let open Utils.Lwt_result.Infix in
      let%lwt _ = Command_utils.setup_databases () in
      pool
      |> Pool_database.Label.create
      |> Lwt_result.lift
      >>= trigger_profile_update_by_tenant
      ||> CCOption.of_result
    | _ -> failwith "Argument missmatch")
;;

let all_profile_update_triggers =
  Sihl.Command.make
    ~name:"trigger_profile_update.send_all"
    ~description:"Send profile update triggers of all tenants"
    (fun args ->
    match args with
    | [] ->
      let open CCFun in
      let open Utils.Lwt_result.Infix in
      Command_utils.setup_databases ()
      >|> Lwt_list.map_s (fun pool -> trigger_profile_update_by_tenant pool)
      ||> CCList.all_ok
      ||> (fun _ -> Ok ()) %> CCOption.of_result
    | _ -> failwith "Argument missmatch")
;;
