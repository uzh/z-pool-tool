let get_or_failwith = Pool_common.Utils.get_or_failwith

let sign_up =
  let help =
    {|<database_label> <email> <password> <firstname> <lastname>
      <language> <terms_accepted>

Provide all fields to sign up a new contact:
        <database_label>      : string
        <email>               : string
        <password>            : string
        <firstname>           : string
        <lastname>            : string
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
      ; language
      ; terms_accepted
      ]
      when CCString.equal terms_accepted "accept" ->
      let%lwt pool = Command_utils.is_available_exn db_pool in
      let%lwt tenant = Pool_tenant.find_by_label pool ||> get_or_failwith in
      let%lwt events =
        let open Cqrs_command.Contact_command.SignUp in
        let language =
          Pool_common.Language.create language |> CCResult.to_opt
        in
        let ({ firstname; lastname; email; _ } as decoded) =
          [ "email", [ email ]
          ; "password", [ password ]
          ; "firstname", [ firstname ]
          ; "lastname", [ lastname ]
          ]
          |> decode
          |> get_or_failwith
        in
        let%lwt token = Email.create_token pool email in
        let%lwt verification_mail =
          Message_template.SignUpVerification.create
            pool
            (CCOption.value ~default:Pool_common.Language.En language)
            tenant
            email
            token
            firstname
            lastname
          ||> get_or_failwith
        in
        decoded
        |> handle token email verification_mail language
        |> get_or_failwith
        |> Lwt.return
      in
      let%lwt () = Lwt_list.iter_s (Pool_event.handle_event pool) events in
      Lwt.return_some ()
    | _ -> Command_utils.failwith_missmatch help)
;;

let trigger_profile_update_by_tenant pool =
  let open Utils.Lwt_result.Infix in
  let* tenant = Pool_tenant.find_by_label pool in
  let* contacts = Contact.find_to_trigger_profile_update pool in
  match contacts with
  | [] -> Lwt_result.return ()
  | contacts ->
    let* create_message =
      Message_template.ProfileUpdateTrigger.prepare_template_list pool tenant
    in
    let* emails =
      CCList.map create_message contacts
      |> CCResult.flatten_l
      |> Lwt_result.lift
    in
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
    >|> Lwt_list.map_s trigger_profile_update_by_tenant
    ||> CCList.all_ok
    ||> get_or_failwith
    ||> fun (_ : unit list) -> Some ())
;;
