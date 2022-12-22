let role_of_string = CCFun.(Format.asprintf "`%s" %> Role.Actor.of_string)
let role_to_string = CCString.replace ~which:`Left ~sub:"`" ~by:"- "

let create =
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

Example: admin.create econ-uzh example@mail.com securePassword Max Muster RecruiterAll
        |}
  in
  Sihl.Command.make
    ~name:"admin.create"
    ~description:"New admin"
    ~help
    (let open Utils.Lwt_result.Infix in
    function
    | [ db_pool; email; password; given_name; name; role ] ->
      let%lwt pool = Command_utils.is_available_exn db_pool in
      let ctx = Pool_tenant.to_ctx pool in
      (match%lwt Service.User.find_by_email_opt ~ctx email with
       | None ->
         let%lwt admin =
           Service.User.create_admin ~ctx ~name ~given_name ~password email
         in
         let%lwt (_ : [> `Admin ] Guard.Authorizable.t) =
           admin
           |> Admin.create
           |> Admin.Guard.Actor.to_authorizable ~ctx
           ||> Pool_common.Utils.get_or_failwith
         in
         let%lwt () =
           let open Guard in
           Persistence.Actor.grant_roles
             ~ctx
             (Uuid.Actor.of_string_exn admin.Sihl_user.id)
             ActorRoleSet.(
               CCList.fold_left (CCFun.flip add) empty [ role_of_string role ])
           ||> CCResult.get_or_failwith
         in
         Lwt.return_some ()
       | Some user when Sihl_user.is_admin user ->
         failwith "The user is already administrator."
       | Some _ -> failwith "The user already exists as contact.")
    | _ -> Command_utils.failwith_missmatch help)
;;

let grant_role =
  let help =
    {|<database_label> <email> <role>

Provide all fields to sign up a new contact:
        <database_label>      : string
        <email>               : string
        <role>                : string

Role: run command "admin.list_roles" to show all possible role patterns

Example: admin.grant_role econ-uzh example@mail.com RecruiterAll
        |}
  in
  Sihl.Command.make
    ~name:"admin.grant_role"
    ~description:"grant role to admin"
    ~help
    (let open Utils.Lwt_result.Infix in
    function
    | [ db_pool; email; role ] ->
      let%lwt pool = Command_utils.is_available_exn db_pool in
      let ctx = Pool_tenant.to_ctx pool in
      (match%lwt Service.User.find_by_email_opt ~ctx email with
       | Some admin when Sihl_user.is_admin admin ->
         let%lwt (_ : [> `Admin ] Guard.Authorizable.t) =
           admin
           |> Admin.create
           |> Admin.Guard.Actor.to_authorizable ~ctx
           ||> Pool_common.Utils.get_or_failwith
         in
         let%lwt () =
           let open Guard in
           Persistence.Actor.grant_roles
             ~ctx
             (Uuid.Actor.of_string_exn admin.Sihl_user.id)
             ActorRoleSet.(
               CCList.fold_left (CCFun.flip add) empty [ role_of_string role ])
           ||> CCResult.get_or_failwith
         in
         Lwt.return_some ()
       | None -> failwith "The user doesn't exist."
       | Some _ -> failwith "The user isn't administrator.")
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
