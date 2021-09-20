module Public = Public

(* This is an example HTTP handler to showcase the usage of CQRS command
   handlers. *)

module Command = Cqrs_command.Participant_command

type request = unit
type response = unit
type handler = request -> response Lwt.t

let urlencoded_of_request = Utils.todo
let response_of_redirect = Utils.todo
let response = Utils.todo

module Tenant_middleware = struct
  let tenant_db_of_request _ =
    (* TODO fetch tenant connection pool based on subdomain *)
    Utils.todo
  ;;
end

let handle_events _ = Utils.todo

let sign_up : handler =
 fun req ->
  let open Lwt.Syntax in
  let tenant_db = Tenant_middleware.tenant_db_of_request req in
  let* allowed_email_suffixes = Settings.allowed_email_suffixes tenant_db in
  let* urlencoded = urlencoded_of_request req in
  let command = Command.SignUp.decode urlencoded in
  match command with
  | Ok command ->
    let events = Command.SignUp.handle ~allowed_email_suffixes command in
    Utils.Database.with_transaction
      (fun conn ->
        match events with
        | Ok events ->
          let* () = handle_events conn events in
          Lwt.return @@ response_of_redirect "/dashboard"
        | Error msg -> Lwt.return @@ response msg)
      tenant_db
  | Error _ -> Lwt.return @@ response "Invalid values provided"
;;
