open CCFun
open Entity

type event =
  | GuardianCacheCleared
  | TenantDatabaseAdded of Pool_database.Label.t
  | TenantDatabaseUpdated of Pool_database.Label.t
  | TenantDatabaseDeleted of Pool_database.Label.t
  | UserBlocked of Argument.user_login_ban
[@@deriving eq, show]

let event_to_key =
  (function
   | GuardianCacheCleared -> "guardiancachecleared"
   | TenantDatabaseAdded _ -> "tenantdatabaseadded"
   | TenantDatabaseUpdated _ -> "tenantdatabaseupdated"
   | TenantDatabaseDeleted _ -> "tenantdatabasedeleted"
   | UserBlocked _ -> "userblocked")
  |> Key.of_string
;;

let handle_event pool : event -> unit Lwt.t = function
  | GuardianCacheCleared as event ->
    let%lwt () =
      Repo.insert
        pool
        { id = Id.create ()
        ; key = event_to_key event
        ; argument = None
        ; created_at = Pool_common.CreatedAt.create ()
        ; updated_at = Pool_common.UpdatedAt.create ()
        }
    in
    Lwt.return_unit
  | ( TenantDatabaseAdded label
    | TenantDatabaseUpdated label
    | TenantDatabaseDeleted label ) as event ->
    let%lwt () =
      Repo.insert
        pool
        { id = Id.create ()
        ; key = event_to_key event
        ; argument =
            label
            |> Pool_database.Label.value
            |> Argument.of_string
            |> CCOption.return
        ; created_at = Pool_common.CreatedAt.create ()
        ; updated_at = Pool_common.UpdatedAt.create ()
        }
    in
    Lwt.return_unit
  | UserBlocked user_login_ban as event ->
    let argument =
      user_login_ban
      |> Argument.yojson_of_user_login_ban
      |> Yojson.Safe.to_string
      |> CCOption.return
    in
    let%lwt () =
      Repo.insert
        pool
        { id = Id.create ()
        ; key = event_to_key event
        ; argument
        ; created_at = Pool_common.CreatedAt.create ()
        ; updated_at = Pool_common.UpdatedAt.create ()
        }
    in
    Lwt.return_unit
;;

module Host = struct
  open EventLog

  type t = Id.t * event [@@deriving eq, show]

  let to_action argument =
    let open Pool_common in
    let open CCOption in
    let fail () = Utils.failwith Message.(Invalid Field.SystemEvent) in
    let find_label =
      flip bind Argument.tenant_opt %> value ~default:(fail ())
    in
    let find_user_login_ban =
      flip bind Argument.user_login_ban_opt %> value ~default:(fail ())
    in
    function
    | "guardiancachecleared" when is_none argument -> GuardianCacheCleared
    | "tenantdatabaseadded" -> TenantDatabaseAdded (find_label argument)
    | "tenantdatabaseupdated" -> TenantDatabaseUpdated (find_label argument)
    | "tenantdatabasedeleted" -> TenantDatabaseDeleted (find_label argument)
    | "userblocked" -> UserBlocked (find_user_login_ban argument)
    | _ -> fail ()
  ;;

  let handle_event pool : Id.t * event -> unit Lwt.t =
    let created_at = Pool_common.CreatedAt.create () in
    let updated_at = Pool_common.UpdatedAt.create () in
    let success event_id identifier =
      { event_id
      ; hostname = identifier
      ; status = Status.Successful
      ; message = None
      ; created_at
      ; updated_at
      }
    in
    let failed event_id identifier message =
      { event_id
      ; hostname = identifier
      ; status = Status.Failed
      ; message = Some message
      ; created_at
      ; updated_at
      }
    in
    function
    | id, GuardianCacheCleared ->
      let () = Guard.Persistence.Cache.clear () in
      let%lwt () = Hostname.get () |> success id |> Repo.EventLog.insert pool in
      Lwt.return_unit
    | id, TenantDatabaseAdded _ ->
      (* TODO: Add connection only without further steps (function adds pool
         tool and guardian databases): Database.Tenant.setup_database ()
         (https://github.com/uzh/pool/pull/139/files#diff-356c5a4af6ccfe63f36bb9529db964a7acbfd89038fc847d21d4f5bf36c30ffcR102) *)
      let%lwt () = Hostname.get () |> success id |> Repo.EventLog.insert pool in
      Lwt.return_unit
    | id, TenantDatabaseUpdated _ ->
      (* TODO: Source: Pool_database.drop_pool (drops pool tool and guardian
         databases) ->
         (https://github.com/uzh/pool/pull/139/files#diff-832e1d88dfc04580161b06f869b12ff6b56c48b4d75c72ca6f2405c0c17c31e5R52) *)
      (* TODO: Add connection only without further steps:
         Database.Tenant.setup_database ()
         (https://github.com/uzh/pool/pull/139/files#diff-356c5a4af6ccfe63f36bb9529db964a7acbfd89038fc847d21d4f5bf36c30ffcR102)*)
      let%lwt () = Hostname.get () |> success id |> Repo.EventLog.insert pool in
      Lwt.return_unit
    | id, TenantDatabaseDeleted _ ->
      (* TODO: Source: Database.drop_pool ->
         (https://github.com/uzh/pool/pull/139/files#diff-832e1d88dfc04580161b06f869b12ff6b56c48b4d75c72ca6f2405c0c17c31e5R52) *)
      let%lwt () = Hostname.get () |> success id |> Repo.EventLog.insert pool in
      Lwt.return_unit
    | id, UserBlocked (_, _, _, _) ->
      let%lwt () =
        Hostname.get ()
        |> failed id "TODO: handle event"
        |> Repo.EventLog.insert pool
      in
      failwith "TODO"
  ;;
end
