open Entity

type event = Created of t [@@deriving show, eq, variants]

let handle_event : event -> unit Lwt.t = function
  | Created t -> Repo.insert Database.Pool.Root.label t
;;

let handle_system_event identifier system_event =
  let open Database.Pool in
  let open EventLog in
  let open Job in
  let create_event_log ?message status =
    create ?message system_event.id (ServiceIdentifier.get identifier) status
    |> Repo.EventLog.insert Root.label
  in
  let success_log () = create_event_log Status.Successful in
  match system_event.job with
  | GuardianCacheCleared ->
    let () = Guard.Persistence.Cache.clear () in
    success_log ()
  | GtxConfigCacheCleared ->
    let () = Gtx_sender.clear_cache () in
    success_log ()
  | I18nPageUpdated ->
    let () = I18n.I18nCache.clear () in
    success_log ()
  | PageScriptsUpdated ->
    let () = Settings.PageScript.clear_cache () in
    success_log ()
  | SmtpAccountUpdated ->
    let () = Email.Service.Cache.clear () in
    success_log ()
  | TenantDatabaseReset database_label ->
    let%lwt () = Tenant.reset database_label in
    success_log ()
  | TenantCacheCleared ->
    let () = Pool_tenant.clear_cache () in
    success_log ()
  | TenantDatabaseCacheCleared ->
    let%lwt () = initialize ~clear:true () in
    success_log ()
;;
