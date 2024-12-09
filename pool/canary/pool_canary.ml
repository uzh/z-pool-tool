let has_to_notify () =
  (* canary will run for all other then dev and testing environments *)
  not Sihl.Configuration.(is_development () || is_test ())
;;

type t =
  { token : string
  ; uri_base : string
  ; project_id : int
  ; project_name : string
  }
[@@deriving fields ~getters]

let t token uri_base project_id project_name =
  { token; uri_base; project_id; project_name }
;;

let schema =
  Conformist.(
    make
      Field.
        [ string
            ~meta:"Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_TOKEN"
        ; string
            ~meta:"Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_API_BASE"
        ; int
            ~meta:"Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_PROJECT_ID"
        ; string
            ~meta:"Used by canary (Exception Notifier) to create an issue in GitLab"
            "GITLAB_PROJECT_NAME"
        ]
      t)
;;

let read_config () = Sihl.Configuration.(read schema)

let notify' ?src ?tags ?labels ?(additional = "") exn trace =
  let config = read_config () in
  let module Gitlab_notify =
    Canary.Notifier.Gitlab (struct
      let token = config |> token
      let uri_base = config |> uri_base
      let project_name = config |> project_name
      let project_id = config |> project_id
    end)
  in
  match%lwt Gitlab_notify.notify ?labels ~additional exn trace with
  | Ok iid ->
    Logs.info ?src (fun m ->
      m ?tags "Successfully reported error to gitlab issue %d." iid);
    Lwt.return_ok ()
  | Error err ->
    Logs.info ?src (fun m -> m ?tags "Unable to report error to gitlab: %s" err);
    Lwt.return_error err
;;

let notify ?src ?tags ?labels ?additional exn trace =
  if has_to_notify ()
  then notify' ?src ?tags ?labels ?additional exn trace
  else (
    let () = Logs.err ?src (fun m -> m ?tags "%s" (Printexc.to_string exn)) in
    Lwt.return_ok ())
;;

let start () =
  if has_to_notify ()
  then (
    let config = read_config () in
    let module Gitlab_notify =
      Canary.Notifier.Gitlab (struct
        let token = config |> token
        let uri_base = config |> uri_base
        let project_name = config |> project_name
        let project_id = config |> project_id
      end)
    in
    Gitlab_notify.connection_test ()
    |> Lwt_result.map_error
         (CCFun.tap (fun err -> Logs.err (fun m -> m "Canary connection: %s" err)))
    |> Lwt.map (CCResult.get_or ~default:()))
  else Lwt.return_unit
;;

let lifecycle = Sihl.Container.create_lifecycle ~start "Canary"

let register () =
  let configuration = Sihl.Configuration.make ~schema () in
  Sihl.Container.Service.create ~configuration lifecycle
;;
