(* This is an example HTTP handler to showcase the usage of CQRS command
   handlers. *)

module Command = Cqrs_command.Participant_command

type request = unit
type response = unit
type handler = request -> response Lwt.t

let urlencoded_of_request = Sihl.todo
let response_of_redirect = Sihl.todo
let response = Sihl.todo

module Tenant_middleware = struct
  let tenant_db_of_request _ =
    (* TODO fetch tenant connection pool based on subdomain *)
    Sihl.todo
  ;;
end

let handle_events _ = Sihl.todo

let sign_up : handler =
 fun req ->
  let open Lwt.Syntax in
  let tenant_db = Tenant_middleware.tenant_db_of_request req in
  let* allowed_email_suffixes = Settings.allowed_email_suffixes tenant_db in
  let* urlencoded = urlencoded_of_request req in
  let command = Command.Sign_up.decode urlencoded in
  match command with
  | Ok command ->
    let events = Command.Sign_up.handle ~allowed_email_suffixes command in
    Sihl.Database.with_transaction
      (fun conn ->
        match events with
        | Ok events ->
          let* () = handle_events conn events in
          Lwt.return @@ response_of_redirect "/dashboard"
        | Error msg -> Lwt.return @@ response msg)
      tenant_db
  | Error _ -> Lwt.return @@ response "Invalid values provided"
;;
