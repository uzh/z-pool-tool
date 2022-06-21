(* Put infrastructure service setup here. This is where you decide which service
   implementation to use. *)
module Migration = Sihl.Database.Migration.MariaDb
module User = Sihl_user.MariaDb
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

  let send ?ctx =
    handle (send ?ctx) (fun email new_recipient ->
        email |> redirected_email new_recipient |> send ?ctx)
  ;;

  let bulk_send ?ctx =
    handle (bulk_send ?ctx) (fun emails new_recipient ->
        emails |> CCList.map (redirected_email new_recipient) |> bulk_send ?ctx)
  ;;
end
