(* Put infrastructure service setup here. This is where you decide which service
   implementation to use. *)
let log_src = Logs.Src.create "email"

module Logs = (val Logs.src_log log_src : Logs.LOG)
module Migration = Sihl.Database.Migration.MariaDb

module User = struct
  include Sihl_user.MariaDb
  include Sihl_user

  let sexp_of_t t = t.id |> fun s -> Sexplib0.Sexp.Atom s
end

module Token = Sihl_token.MariaDb
module PasswordReset = Sihl_user.Password_reset.MakeMariaDb (Token)
module EmailTemplate = Sihl_email.Template.MariaDb
module Queue = Sihl_queue.MariaDb
module BlockingEmail = Sihl_email.Smtp
module Storage = Sihl_storage.MariaDb

module Email = struct
  (* NOTE: The mail intercepter will most likely be implemented within SIHL
     4.0.0 *)
  include Sihl_email.Queued (Queue) (BlockingEmail)

  let redirected_email
    new_recipient
    (Sihl_email.{ recipient; subject; _ } as email)
    =
    let subject =
      Format.asprintf "[Pool Tool] %s (original to: %s)" subject recipient
    in
    Sihl_email.{ email with subject; recipient = new_recipient }
  ;;

  let handle
    ?(without_email_fcn = CCFun.const Lwt.return_unit)
    production_fcn
    else_fcn
    email
    =
    match Sihl.Configuration.is_production () with
    | true -> production_fcn email
    | false ->
      (match Sihl.Configuration.read_string "TEST_EMAIL" with
       | None ->
         Logs.err (fun m ->
           m
             "%s"
             "Sending email intercepted! As no redirect email is specified \
              it/they wont be sent. Please define environment variable \
              'TEST_EMAIL'.");
         without_email_fcn email
       | Some new_recipient ->
         Logs.info (fun m ->
           m
             "Sending email intercepted. Sending email(s) to new recipient \
              ('%s')"
             new_recipient);
         else_fcn email new_recipient)
  ;;

  let send ?sender ?ctx email =
    Logs.info (fun m -> m "Send email to %s" email.Sihl_email.recipient);
    let email =
      match sender with
      | Some sender -> Sihl_email.{ email with sender }
      | None -> email
    in
    let%lwt () =
      handle
        (send ?ctx)
        (fun email new_recipient ->
          email |> redirected_email new_recipient |> send ?ctx)
        email
    in
    Logs.info (fun m -> m "Email sent");
    Lwt.return ()
  ;;

  let bulk_send ?sender ?ctx emails =
    let emails =
      List.map
        (fun (email : Sihl_email.t) ->
          match sender with
          | Some sender -> Sihl_email.{ email with sender }
          | None -> email)
        emails
    in
    Logs.info (fun m -> m "Send %d emails" (CCList.length emails));
    let%lwt () =
      handle
        (bulk_send ?ctx)
        (fun emails new_recipient ->
          emails
          |> CCList.map (redirected_email new_recipient)
          |> bulk_send ?ctx)
        emails
    in
    Logs.info (fun m -> m "%d emails sent" (CCList.length emails));
    Lwt.return ()
  ;;
end
