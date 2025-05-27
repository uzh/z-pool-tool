module Dynparam = Database.Dynparam
open CCFun.Infix

let additional_joins =
  [ {sql|
    LEFT JOIN (
      SELECT
        contact_uuid,
        MAX(created_at) AS latest_notification,
        COUNT(*) AS notification_count
      FROM
        pool_contact_deactivation_notification
      GROUP BY
        contact_uuid
    ) pcdn ON pool_contacts.user_uuid = pcdn.contact_uuid
  |sql}
  ]
;;

let find_to_warn_about_inactivity_request latest_notification_timestamps =
  let where =
    Format.asprintf
      {sql|
        WHERE
          (
            pool_contacts.paused = 0
            AND pool_contacts.disabled = 0
            AND user_users.status = "active"
            AND pool_contacts.email_verified IS NOT NULL
            AND pool_contacts.import_pending = 0
          )
          AND 
          (
            (
              last_sign_in_at <= NOW() - INTERVAL ? SECOND
              AND 
                (
                  pcdn.notification_count = 0 
                  OR 
                  pcdn.notification_count IS NULL
                )
            ) OR (
              pcdn.latest_notification <= NOW() - INTERVAL %s SECOND 
              AND
              pcdn.notification_count < ?
              )
          ) LIMIT 100
      |sql}
      latest_notification_timestamps
  in
  Contact.Repo.find_request_sql ~additional_joins where
;;

let find_to_warn_about_inactivity pool warn_after =
  match warn_after with
  | [] -> Lwt.return []
  | warn_after ->
    let open Dynparam in
    let open Caqti_request.Infix in
    let warn_after_s =
      CCList.map
        (Ptime.Span.to_int_s %> CCOption.get_exn_or "Invalid time span")
        warn_after
    in
    let dyn =
      CCList.fold_left (fun dyn span -> dyn |> add Caqti_type.int span) empty warn_after_s
    in
    let sql =
      match warn_after with
      | [] -> failwith "Emtpy list provided"
      | [ _ ] -> "?"
      | _ :: tl_warn ->
        (* Ignoring the first element, as this case is hardcoded in the first condition *)
        tl_warn
        |> CCList.mapi (fun i _ ->
          Format.asprintf "WHEN pcdn.notification_count = %i THEN ?" (i + 1))
        |> CCString.concat "\n"
        |> Format.asprintf "( CASE %s ELSE ? END )"
    in
    (* Adding the last timestampt as the default case *)
    let dyn =
      add Caqti_type.int (CCList.rev warn_after_s |> CCList.hd) dyn
      |> add Caqti_type.int (CCList.length warn_after)
    in
    let request = find_to_warn_about_inactivity_request sql in
    let (Pack (pt, pv)) = dyn in
    let request = request |> pt ->* Contact.Repo.t in
    Database.collect pool request pv
;;

let find_to_disable pool disable_after n_reminders =
  let open Dynparam in
  let request where =
    Format.asprintf
      {sql|
        WHERE
          (
            pool_contacts.paused = 0
            AND pool_contacts.disabled = 0
            AND user_users.status = "active"
            AND pool_contacts.email_verified IS NOT NULL
            AND pool_contacts.import_pending = 0
          )
          AND %s
          LIMIT 100
      |sql}
      where
    |> Contact.Repo.find_request_sql ~additional_joins
  in
  let needs_reminders =
    {sql|
      pcdn.latest_notification <= NOW() - INTERVAL $1 SECOND
      AND pcdn.notification_count = $2
  |sql}
  in
  let check_last_login =
    {sql|
      last_sign_in_at <= NOW() - INTERVAL $1 SECOND
   |sql}
  in
  let Pack (pt, pv), where =
    let disable_after =
      disable_after |> Ptime.Span.to_int_s |> CCOption.get_exn_or "Invalid time span"
    in
    let dyn = empty |> add Caqti_type.int disable_after in
    match n_reminders with
    | 0 -> dyn, check_last_login
    | _ -> add Caqti_type.int n_reminders dyn, needs_reminders
  in
  let request =
    let open Caqti_request.Infix in
    request where |> pt ->* Contact.Repo.t
  in
  Database.collect pool request pv
;;
