open Entity

type event = Created of t [@@deriving show, eq, variants]

let handle_event : event -> unit Lwt.t = function
  | Created t -> Repo.insert Database.root t
;;

let handle_system_event identifier system_event =
  let open Utils.Lwt_result.Infix in
  let open EventLog in
  let pool = Database.root in
  let create_event_log ?message status =
    create ?message system_event.id (ServiceIdentifier.get identifier) status
    |> Repo.EventLog.insert pool
  in
  let success_log () = create_event_log Status.Successful in
  let error_log message = create_event_log ~message Status.Successful in
  let handle_result = function
    | Ok _ -> success_log ()
    | Error err ->
      err |> Pool_common.(Utils.error_to_string Language.En) |> error_log
  in
  let add_pool label = Database.Tenant.(find label |>> add) in
  let open Job in
  match system_event.job with
  | GuardianCacheCleared ->
    let () = Guard.Persistence.Cache.clear () in
    success_log ()
  | I18nPageUpdated ->
    let () = I18n.I18nPageCache.clear () in
    success_log ()
  | SmtpAccountUpdated ->
    let () = Email.Service.Cache.clear () in
    success_log ()
  | TenantDatabaseAdded database_label ->
    let%lwt () = Database.drop_pool database_label in
    add_pool database_label >|> handle_result
  | TenantDatabaseUpdated database_label ->
    let%lwt () = Database.drop_pool database_label in
    add_pool database_label >|> handle_result
  | TenantDatabaseDeleted database_label ->
    let%lwt () = Database.drop_pool database_label in
    success_log ()
  | TenantDatabaseCacheCleared ->
    let () = Pool_tenant.clear_cache () in
    success_log ()
;;
