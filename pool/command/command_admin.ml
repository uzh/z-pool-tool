let parse_role role =
  role
  |> Format.asprintf {|[["%s"]]|}
  |> Yojson.Safe.from_string
  |> Guard.ActorRoleSet.of_yojson
  |> CCResult.get_or_failwith
  |> Guard.ActorRoleSet.elements
;;

let create =
  Sihl.Command.make
    ~name:"admin.create"
    ~description:"New admin"
    ~help:"<Pool_database> <email> <password> <firstname> <lastname> <role>"
    (fun args ->
    let return = Lwt.return_some () in
    let help_text =
      {|Provide all fields to sign up a new contact:
    <Pool_database>       : string
    <email>               : string
    <password>            : string
    <firstname>           : string
    <lastname>            : string
    <role>                : string

Example: admin.create econ-uzh example@mail.com securePassword Max Muster RecruiterAll
          |}
    in
    match args with
    | [ db_pool; email; password; given_name; name; role ] ->
      let db_pool =
        Pool_database.Label.create db_pool
        |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
        |> CCResult.get_or_failwith
      in
      Database.Root.setup ();
      let%lwt available_pools = Database.Tenant.setup () in
      if CCList.mem ~eq:Pool_database.Label.equal db_pool available_pools
      then (
        let ctx = Pool_tenant.to_ctx db_pool in
        let%lwt user = Service.User.find_by_email_opt ~ctx email in
        let role = parse_role role in
        match user with
        | None ->
          let%lwt admin =
            Service.User.create_admin ~ctx ~name ~given_name ~password email
          in
          let%lwt (_ : [> `Admin ] Guard.Authorizable.t) =
            admin
            |> Admin.create
            |> Admin.Guard.Actor.to_authorizable ~ctx
            |> Lwt.map Pool_common.Utils.get_or_failwith
          in
          let%lwt () =
            Guard.Persistence.Actor.grant_roles
              ~ctx
              (Guard.Uuid.Actor.of_string_exn admin.Sihl_user.id)
              Guard.ActorRoleSet.(CCList.fold_left (CCFun.flip add) empty role)
            |> Lwt.map CCResult.get_or_failwith
          in
          return
        | Some user when Sihl_user.is_admin user ->
          print_endline "The user is already administrator.";
          return
        | Some _ -> return)
      else (
        print_endline
          (Format.asprintf
             "The specified database pool %s is not available (%s)."
             ([%show: Pool_database.Label.t] db_pool)
             ([%show: Pool_database.Label.t list] available_pools));
        return)
    | _ ->
      print_endline help_text;
      return)
;;

let grant_role =
  Sihl.Command.make
    ~name:"admin.grant_role"
    ~description:"grant role to admin"
    ~help:"<Pool_database> <email> <role>"
    (fun args ->
    let return = Lwt.return_some () in
    let help_text =
      {|Provide all fields to sign up a new contact:
    <Pool_database>       : string
    <email>               : string
    <role>                : string

Example: admin.grant_role econ-uzh example@mail.com RecruiterAll
          |}
    in
    match args with
    | [ db_pool; email; role ] ->
      let db_pool =
        Pool_database.Label.create db_pool
        |> CCResult.map_err Pool_common.(Utils.error_to_string Language.En)
        |> CCResult.get_or_failwith
      in
      Database.Root.setup ();
      let%lwt available_pools = Database.Tenant.setup () in
      if CCList.mem ~eq:Pool_database.Label.equal db_pool available_pools
      then (
        let ctx = Pool_tenant.to_ctx db_pool in
        let%lwt user = Service.User.find_by_email_opt ~ctx email in
        let role = parse_role role in
        match user with
        | Some admin when Sihl_user.is_admin admin ->
          let%lwt (_ : [> `Admin ] Guard.Authorizable.t) =
            admin
            |> Admin.create
            |> Admin.Guard.Actor.to_authorizable ~ctx
            |> Lwt.map Pool_common.Utils.get_or_failwith
          in
          let%lwt () =
            Guard.Persistence.Actor.grant_roles
              ~ctx
              (Guard.Uuid.Actor.of_string_exn admin.Sihl_user.id)
              Guard.ActorRoleSet.(CCList.fold_left (CCFun.flip add) empty role)
            |> Lwt.map CCResult.get_or_failwith
          in
          return
        | None | Some _ ->
          print_endline "The user isn't administrator.";
          return)
      else (
        print_endline "The specified database pool is not available.";
        return)
    | _ ->
      print_endline help_text;
      return)
;;
