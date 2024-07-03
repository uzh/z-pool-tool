let log_request_with_ip ~src message req tags email =
  let open Opium in
  let open Request in
  let ip =
    Headers.get req.headers "X-Real-IP"
    |> CCOption.value ~default:"X-Real-IP not found"
  in
  let email =
    email
    |> CCOption.map_or ~default:"" (fun email ->
      Format.asprintf " %a" Pool_user.EmailAddress.pp email)
  in
  let msg = Format.asprintf "%s: %s%s" message ip email in
  Logs.warn ~src (fun m -> m "%s" msg ~tags)
;;
