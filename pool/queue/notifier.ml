open CCFun
open Utils.Lwt_result.Infix
open Sihl.Contract.Queue

let src = Logs.Src.create "queue.notifier"

type config =
  { token : string
  ; uri_base : string
  ; project_id : int
  ; project_name : string
  }

let config token uri_base project_id project_name =
  { token; uri_base; project_id; project_name }
;;

let schema =
  Conformist.(
    make
      Field.
        [ string
            ~meta:
              "Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_TOKEN"
        ; string
            ~meta:
              "Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_API_BASE"
        ; int
            ~meta:
              "Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_PROJECT_ID"
        ; string
            ~meta:
              "Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_PROJECT_NAME"
        ]
      config)
;;

let before_start () =
  if Sihl.Configuration.is_production ()
  then (
    (* Validate configuration variables for production environment*)
    let (_ : config) = Sihl.Configuration.(read schema) in
    ())
  else ()
;;

let create_external_link pool_url =
  Sihl.Web.externalize_path
  %> Format.asprintf "http://%s%s" (Pool_tenant.Url.value pool_url)
;;

let job_reporter
  ({ id
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
    instance)
  =
  match status, last_error with
  | (Failed | Pending), Some last_error ->
    let config = Sihl.Configuration.(read schema) in
    let module Gitlab_notify =
      Canary.Notifier.Gitlab (struct
        let token = config.token
        let uri_base = config.uri_base
        let project_name = config.project_name
        let project_id = config.project_id
      end)
    in
    let database_label = Pool_database.of_ctx_opt ctx in
    let tags =
      CCOption.map_or
        ~default:Logs.Tag.empty
        Pool_database.Logger.Tags.create
        database_label
    in
    let%lwt link =
      let default = "Couldn't generate Link" in
      let path = Format.asprintf "/admin/settings/queue/%s" id in
      match database_label with
      | Some database_label when Pool_database.(Label.equal root database_label)
        ->
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
    Gitlab_notify.notify ~additional (Exception last_error) ""
    ||> (function
    | Ok iid ->
      Logs.info ~src (fun m ->
        m ~tags "Successfully reported error to gitlab as issue %d." iid)
    | Error err ->
      Logs.err ~src (fun m ->
        m ~tags "Unable to report error to gitlab: %s" err))
  | (Succeeded | Pending | Cancelled | Failed), _ -> Lwt.return_unit
;;
