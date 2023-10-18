let log_request_with_ip ~src message req tags email =
  let open Opium in
  let open Request in
  let ip =
    Headers.get req.headers "X-Real-IP"
    |> CCOption.value ~default:"X-Real-IP not found"
  in
  Logs.warn ~src (fun m ->
    m "%s: %s %s" message ip (Pool_user.EmailAddress.value email) ~tags)
;;
