open CCFun
open Utils.Lwt_result.Infix

let src = Logs.Src.create "queue.notifier"

let create_external_link pool_url =
  Sihl.Web.externalize_path
  %> Format.asprintf "http://%s%s" (Pool_tenant.Url.value pool_url)
;;

let job_reporter
  ({ Entity.Instance.id
   ; tries
   ; max_tries
   ; status
   ; last_error
   ; database_label
   ; _
   } as instance)
  =
  let open Entity.Status in
  match status, last_error with
  | (Failed | Pending), Some last_error when tries >= max_tries ->
    let tags = Database.Logger.Tags.create database_label in
    let%lwt link =
      let default = "Couldn't generate Link" in
      let path = [%string "/admin/settings/queue/%{Entity.Id.value id}"] in
      if Database.(Label.equal root database_label)
      then
        Sihl.Configuration.read_string "PUBLIC_URL"
        |> CCOption.map_or
             ~default
             (Pool_tenant.Url.of_string %> flip create_external_link path)
        |> Lwt.return
      else
        Pool_tenant.find_by_label database_label
        >|+ (fun { Pool_tenant.url; _ } -> url)
        ||> CCResult.map_or ~default (flip create_external_link path)
    in
    let additional =
      Format.asprintf
        "An error occured in the job worker:\nInstance: %s\nLink: <%s>"
        ([%show: Entity.Instance.t] instance)
        link
    in
    Pool_canary.notify ~src ~tags ~additional (Failure last_error) ""
    |> Lwt.map (CCResult.get_or ~default:())
  | (Succeeded | Pending | Cancelled | Failed), _ -> Lwt.return_unit
;;
