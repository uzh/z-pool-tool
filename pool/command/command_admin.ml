open CCFun.Infix
module BaseGuard = Guard

let get_or_failwith = Pool_common.Utils.get_or_failwith
let role_of_string = CCFun.(Format.asprintf "`%s" %> Role.Role.of_string)

let role_uuid_of_string role uuid =
  Format.asprintf "`%s (%s)" role uuid |> Role.Actor.of_string
;;

let role_to_string =
  let open CCFun in
  CCString.replace ~which:`Left ~sub:"`" ~by:"- "
  %> CCString.replace ~which:`Right ~sub:"(" ~by:""
  %> CCString.replace ~which:`Right ~sub:")" ~by:""
;;

let grant_role ctx admin (role, target_uuid) =
  let open Utils.Lwt_result.Infix in
  let%lwt (_ : Guard.Actor.t) =
    admin |> Admin.Guard.Actor.to_authorizable ~ctx ||> get_or_failwith
  in
  let open Guard in
  ActorRole.create
    ?target_uuid
    (Uuid.Actor.of_string_exn admin.Admin.user.Sihl_user.id)
    role
  |> Persistence.ActorRole.upsert ~ctx
  ||> CCFun.tap (fun _ -> Persistence.Cache.clear ())
;;

let create =
  let create_and_grant_role_exn pool email password given_name name role =
    let ctx = Database.to_ctx pool in
    match%lwt Service.User.find_by_email_opt ~ctx email with
    | None ->
      let open Pool_user in
      let email = EmailAddress.create email |> get_or_failwith in
      let firstname = Firstname.create given_name |> get_or_failwith in
      let lastname = Lastname.create name |> get_or_failwith in
      let password = Password.create password |> get_or_failwith in
      let admin : Admin.create =
        { id = None
        ; Admin.email
        ; password
        ; firstname
        ; lastname
        ; roles = [ role ]
        }
      in
      let%lwt () =
        Admin.Created admin |> Pool_event.(admin %> handle_event pool)
      in
      Lwt.return_some ()
    | Some user when Sihl_user.is_admin user ->
      failwith "The user already exists as admin, use the 'grant_role' command."
    | Some _ -> failwith "The user already exists as contact."
  in
  let help =
    {|<database_label> <email> <password> <firstname> <lastname> <role>

Provide all fields to sign up a new contact:
        <database_label>      : string
        <email>               : string
        <password>            : string
        <firstname>           : string
        <lastname>            : string
        <role>                : string

Role: run command "admin.list_roles" to show all possible role patterns

NOTE: There is NO check if the UUID of a role is correct for CLI commands.

Example: admin.create econ-uzh example@mail.com securePassword Max Muster RecruiterAll
        |}
  in
  Sihl.Command.make
    ~name:"admin.create"
    ~description:"New admin"
    ~help
    (function
    | [ db_pool; email; password; given_name; name; role ] ->
      let%lwt pool = Command_utils.is_available_exn db_pool in
      (role_of_string role, None)
      |> create_and_grant_role_exn pool email password given_name name
    | [ db_pool; email; password; given_name; name; role; uuid ] ->
      let%lwt pool = Command_utils.is_available_exn db_pool in
      let target_uuid = Guard.Uuid.Target.of_string_exn uuid in
      (role_of_string role, Some target_uuid)
      |> create_and_grant_role_exn pool email password given_name name
    | _ -> Command_utils.failwith_missmatch help)
;;

let create_root_admin =
  let create_exn email password given_name name =
    let ctx = Database.to_ctx Database.root in
    match%lwt Service.User.find_by_email_opt ~ctx email with
    | None ->
      let%lwt () =
        let open Admin in
        let open Pool_user in
        let admin_id = Id.create () in
        Created
          { id = Some admin_id
          ; email = email |> EmailAddress.of_string
          ; password = password |> Password.create |> CCResult.get_exn
          ; firstname = given_name |> Firstname.of_string
          ; lastname = name |> Lastname.of_string
          ; roles = [ `Operator, None ]
          }
        |> handle_event ~tags:Database.(Logger.Tags.create root) Database.root
      in
      Lwt.return_some ()
    | Some _ -> failwith "The user already exists."
  in
  let help =
    {|<email> <password> <firstname> <lastname>

Provide all fields to sign up a new contact:
        <email>               : string
        <password>            : string
        <firstname>           : string
        <lastname>            : string

Example: admin.root.create example@mail.com securePassword Max Muster
        |}
  in
  Sihl.Command.make
    ~name:"admin.root.create"
    ~description:"New admin"
    ~help
    (function
    | [ email; password; given_name; name ] ->
      let%lwt (_ : Database.status) = Database.Root.setup () in
      create_exn email password given_name name
    | _ -> Command_utils.failwith_missmatch help)
;;

let grant_role =
  let grant_if_admin pool email role =
    let open Utils.Lwt_result.Infix in
    let ctx = Database.to_ctx pool in
    let%lwt admin =
      email
      |> Pool_user.EmailAddress.of_string
      |> Admin.find_by_email pool
      ||> get_or_failwith
    in
    let%lwt () = grant_role ctx admin role in
    Lwt.return_some ()
  in
  let help =
    {|<database_label> <email> <role>

Provide all fields to sign up a new contact:
        <database_label>      : string
        <email>               : string
        <role>                : string

Role: run command "admin.list_roles" to show all possible role patterns

NOTE: There is NO check if the UUID of a role is correct for CLI commands.

Example: admin.grant_role econ-uzh example@mail.com RecruiterAll
        |}
  in
  Sihl.Command.make
    ~name:"admin.grant_role"
    ~description:"grant role to admin"
    ~help
    (function
    | [ db_pool; email; role ] ->
      let%lwt pool = Command_utils.is_available_exn db_pool in
      (role |> role_of_string, None) |> grant_if_admin pool email
    | [ db_pool; email; role; uuid ] ->
      let%lwt pool = Command_utils.is_available_exn db_pool in
      let target_uuid = Guard.Uuid.Target.of_string_exn uuid in
      (role_of_string role, Some target_uuid) |> grant_if_admin pool email
    | _ -> Command_utils.failwith_missmatch help)
;;

let list_roles =
  Command_utils.make_no_args
    "admin.list_roles"
    "Shows a list of possible administrator roles."
    (fun () ->
       Role.Actor.all
       |> CCList.map [%show: Role.Actor.t]
       |> CCList.map role_to_string
       |> CCString.concat "\n  "
       |> Format.asprintf "Possible roles:\n  %s"
       |> print_endline;
       Lwt.return_some ())
;;
