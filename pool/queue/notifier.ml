open CCFun
open Utils.Lwt_result.Infix

let src = Logs.Src.create "queue.notifier"

let create_external_link pool_url =
  Sihl.Web.externalize_path
  %> Format.asprintf "http://%s%s" (Pool_tenant.Url.value pool_url)
;;

let job_reporter
  ({ Entity.id
   ; name
   ; tries
   ; next_run_at
   ; max_tries
   ; status
   ; last_error
   ; last_error_at
   ; tag
   ; ctx
   ; _
   } :
    Entity.instance)
  =
  let open Entity.Status in
  match status, last_error with
  | (Failed | Pending), Some last_error when tries >= max_tries ->
    let database_label = Database.of_ctx_opt ctx in
    let tags =
      CCOption.map_or
        ~default:Logs.Tag.empty
        Database.Logger.Tags.create
        database_label
    in
    let%lwt link =
      let default = "Couldn't generate Link" in
      let path = Format.asprintf "/admin/settings/queue/%s" id in
      match database_label with
      | Some database_label when Database.(Label.equal root database_label) ->
        Sihl.Configuration.read_string "PUBLIC_URL"
        |> CCOption.map_or
             ~default
             (Pool_tenant.Url.of_string %> flip create_external_link path)
        |> Lwt.return
      | Some database_label ->
        Pool_tenant.find_by_label database_label
        >|+ (fun { Pool_tenant.url; _ } -> url)
        ||> CCResult.map_or ~default (flip create_external_link path)
      | None -> failwith ""
    in
    let additional =
      Format.asprintf
        "An error occured in the job worker:\n\
         Name: %s\n\
         Uuid: '%s'\n\
         Tries: %d/%d\n\
         Last Error At: %s\n\
         Next Run At: %s\n\
         Tags: %s\n\
         Link: <%s>"
        name
        id
        tries
        max_tries
        ([%show: Ptime.t option] last_error_at)
        ([%show: Ptime.t] next_run_at)
        ([%show: string option] tag)
        link
    in
    Pool_canary.notify ~src ~tags ~additional (Failure last_error) ""
    |> Lwt.map (CCResult.get_or ~default:())
  | (Succeeded | Pending | Cancelled | Failed), _ -> Lwt.return_unit
;;
