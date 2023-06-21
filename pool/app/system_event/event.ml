open CCFun
open Entity

type event = Created of t [@@deriving show, eq, variants]

let handle_event : event -> unit Lwt.t = function
  | Created t -> Repo.insert Pool_database.root t
;;

let handle_system_event system_event =
  let open Utils.Lwt_result.Infix in
  let pool = Pool_database.root in
  let created_at = Pool_common.CreatedAt.create () in
  let updated_at = Pool_common.UpdatedAt.create () in
  let success identifier =
    EventLog.
      { event_id = system_event.id
      ; service_identifier = identifier
      ; status = Status.Successful
      ; message = None
      ; created_at
      ; updated_at
      }
  in
  let failed identifier message =
    EventLog.
      { event_id = system_event.id
      ; service_identifier = identifier
      ; status = Status.Failed
      ; message = Some message
      ; created_at
      ; updated_at
      }
  in
  let handle_result =
    let identifier = EventLog.ServiceIdentifier.get () in
    let insert = Repo.EventLog.insert pool in
    function
    | Ok _ -> success identifier |> insert
    | Error err ->
      Pool_common.(Utils.error_to_string Language.En) err
      |> failed identifier
      |> insert
  in
  let add_pool database_label =
    Pool_tenant.find_database_by_label database_label
    |>> Database.Tenant.(setup_tenant ~run_functions:setup_functions)
  in
  let open Job in
  match system_event.job with
  | GuardianCacheCleared ->
    let () = Guard.Persistence.Cache.clear () in
    let%lwt () =
      EventLog.ServiceIdentifier.get () |> success |> Repo.EventLog.insert pool
    in
    Lwt.return_unit
  | TenantDatabaseAdded database_label ->
    add_pool database_label >|> handle_result
  | TenantDatabaseUpdated database_label ->
    let%lwt () = Pool_database.drop_pool database_label in
    add_pool database_label >|> handle_result
  | TenantDatabaseDeleted database_label ->
    let%lwt () = Pool_database.drop_pool database_label in
    EventLog.ServiceIdentifier.get () |> success |> Repo.EventLog.insert pool
;;
