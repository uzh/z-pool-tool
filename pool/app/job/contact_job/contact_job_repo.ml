module Dynparam = Database.Dynparam

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
              pcdn.latest_notification <= NOW() - INTERVAL ( %s ) SECOND 
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
    let open Caqti_request.Infix in
    let warn_after_s =
      CCList.map
        (fun span -> Ptime.Span.to_int_s span |> CCOption.get_exn_or "Invalid time span")
        warn_after
    in
    let dyn =
      let open Dynparam in
      CCList.fold_left (fun dyn span -> dyn |> add Caqti_type.int span) empty warn_after_s
    in
    let sql =
      (* Ignoring the first element, as this is hardcoded in the query *)
      warn_after
      |> CCList.tl
      |> CCList.mapi (fun i _ ->
        Format.asprintf "WHEN pcdn.notification_count = %i THEN ?" (i + 1))
    in
    (* Adding the last timestampt as the default case *)
    let dyn =
      let open Dynparam in
      add Caqti_type.int (CCList.rev warn_after_s |> CCList.hd) dyn
      |> add Caqti_type.int (CCList.length warn_after)
    in
    let sql = sql |> CCString.concat "\n" |> Format.asprintf "CASE %s ELSE ? END" in
    let request = find_to_warn_about_inactivity_request sql in
    let (Dynparam.Pack (pt, pv)) = dyn in
    let request = request |> pt ->* Contact.Repo.t in
    Database.collect pool request pv
;;

let find_to_disable pool disable_after n_reminders =
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
  let dyn, where =
    let disable_after =
      disable_after |> Ptime.Span.to_int_s |> CCOption.get_exn_or "Invalid time span"
    in
    let dyn = Dynparam.empty |> Dynparam.add Caqti_type.int disable_after in
    match n_reminders with
    | 0 -> dyn, check_last_login
    | _ -> Dynparam.add Caqti_type.int n_reminders dyn, needs_reminders
  in
  let (Dynparam.Pack (pt, pv)) = dyn in
  let request =
    let open Caqti_request.Infix in
    request where |> pt ->* Contact.Repo.t
  in
  Database.collect pool request pv
;;
