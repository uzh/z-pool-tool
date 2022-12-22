let get_or_failwith = Pool_common.Utils.get_or_failwith

let sign_up =
  let help =
    {|<database_label> <email> <password> <firstname> <lastname>
      <recruitment_channel> <language> <terms_accepted>

Provide all fields to sign up a new contact:
        <database_label>      : string
        <email>               : string
        <password>            : string
        <firstname>           : string
        <lastname>            : string
        <recruitment_channel> : string of 'friend', 'online', 'lecture', 'mailing'
        <language>            : string of 'DE', 'EN'
        <terms_accepted>      : string 'accept' everything else is treated as declined

Note: Make sure 'accept' is added as final argument, otherwise signup fails.

Example: contact.signup econ-uzh example@mail.com securePassword Max Muster online
       |}
  in
  Sihl.Command.make
    ~name:"contact.signup"
    ~description:"New contact signup"
    ~help
    (let open Utils.Lwt_result.Infix in
    function
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
      let%lwt pool = Command_utils.is_available_exn db_pool in
      let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
      let events =
        let open CCResult.Infix in
        let open Cqrs_command.Contact_command.SignUp in
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
        |> get_or_failwith
      in
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event pool) events in
      Lwt.return_some ()
    | _ -> Command_utils.failwith_missmatch help)
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
          create_message contact template |> Lwt_result.return)
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
  Command_utils.make_pool_specific
    "trigger_profile_update.send"
    "Send profile update triggers of all tenants"
    (fun pool ->
    let open Utils.Lwt_result.Infix in
    pool
    |> trigger_profile_update_by_tenant
    ||> get_or_failwith
    ||> CCOption.some)
;;

let all_profile_update_triggers =
  Command_utils.make_no_args
    "trigger_profile_update.send_all"
    "Send profile update triggers of all tenants"
    (fun () ->
    let open Utils.Lwt_result.Infix in
    Command_utils.setup_databases ()
    >|> Lwt_list.map_s (fun pool -> trigger_profile_update_by_tenant pool)
    ||> CCList.all_ok
    ||> get_or_failwith
    ||> fun (_ : unit list) -> Some ())
;;
