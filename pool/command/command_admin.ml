open CCFun.Infix
module BaseGuard = Guard

let get_or_failwith = Pool_common.Utils.get_or_failwith
let role_of_string = CCFun.(Format.asprintf "`%s" %> Role.Actor.of_string)

let role_uuid_of_string role uuid =
  Format.asprintf "`%s (%s)" role uuid |> Role.Actor.of_string
;;

let role_to_string =
  let open CCFun in
  CCString.replace ~which:`Left ~sub:"`" ~by:"- "
  %> CCString.replace ~which:`Right ~sub:"(" ~by:""
  %> CCString.replace ~which:`Right ~sub:")" ~by:""
;;

let grant_role ctx admin role =
  let open Utils.Lwt_result.Infix in
  let%lwt (_ : [> `Admin ] Guard.Actor.t) =
    admin
    |> Admin.create
    |> Admin.Guard.Actor.to_authorizable ~ctx
    ||> Pool_common.Utils.get_or_failwith
  in
  let open Guard in
  Persistence.Actor.grant_roles
    ~ctx
    (Uuid.Actor.of_string_exn admin.Sihl_user.id)
    RoleSet.(CCList.fold_left (CCFun.flip add) empty [ role ])
  >|- (fun (_ : string) ->
        "Invalid Role: check possible role patterns (admin.list_roles)")
  ||> CCFun.tap (fun _ -> Persistence.Cache.clear ())
  ||> CCResult.get_or_failwith
;;

let create =
  let create_and_grant_role_exn pool email password given_name name role =
    let ctx = Pool_database.to_ctx pool in
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
        ; roles = Some (Guard.RoleSet.singleton role)
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
      role_of_string role
      |> create_and_grant_role_exn pool email password given_name name
    | [ db_pool; email; password; given_name; name; role; uuid ] ->
      let%lwt pool = Command_utils.is_available_exn db_pool in
      role_uuid_of_string role uuid
      |> create_and_grant_role_exn pool email password given_name name
    | _ -> Command_utils.failwith_missmatch help)
;;

let create_root_admin =
  let create_exn email password given_name name =
    let ctx = Pool_database.to_ctx Pool_database.root in
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
          ; roles =
              Some (BaseGuard.RoleSet.of_list [ `Operator; `ManageOperators ])
          }
        |> handle_event
             ~tags:Pool_database.(Logger.Tags.create root)
             Pool_database.root
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
      let%lwt () = Database.Root.setup () in
      create_exn email password given_name name
    | _ -> Command_utils.failwith_missmatch help)
;;

let grant_role =
  let grant_if_admin pool email role =
    let ctx = Pool_database.to_ctx pool in
    match%lwt Service.User.find_by_email_opt ~ctx email with
    | Some admin when Sihl_user.is_admin admin ->
      let%lwt () = grant_role ctx admin role in
      Lwt.return_some ()
    | Some _ -> failwith "The user isn't administrator."
    | None -> failwith "The user doesn't exist, use the 'create' command."
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
      role |> role_of_string |> grant_if_admin pool email
    | [ db_pool; email; role; uuid ] ->
      let%lwt pool = Command_utils.is_available_exn db_pool in
      role_uuid_of_string role uuid |> grant_if_admin pool email
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
