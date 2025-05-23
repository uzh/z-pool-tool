open CCFun.Infix
open Repo_entity
module Dynparam = Database.Dynparam

let sql_select_columns =
  (Entity.Id.sql_select_fragment ~field:"pool_assignments.uuid"
   :: Contact.Repo.sql_select_columns)
  @ [ "pool_assignments.no_show"
    ; "pool_assignments.participated"
    ; "pool_assignments.matches_filter"
    ; "pool_assignments.canceled_at"
    ; "pool_assignments.marked_as_deleted"
    ; "pool_assignments.external_data_id"
    ; "pool_assignments.reminder_manually_last_sent_at"
    ; "pool_assignments.created_at"
    ; "pool_assignments.updated_at"
    ]
;;

let joins =
  Format.asprintf
    {sql|
      INNER JOIN pool_contacts
        ON pool_contacts.user_uuid = pool_assignments.contact_uuid
      %s
    |sql}
    Contact.Repo.joins
;;

let joins_session =
  {sql| INNER JOIN pool_sessions ON pool_sessions.uuid = pool_assignments.session_uuid |sql}
;;

let not_deleted_condition = "pool_assignments.marked_as_deleted = 0"
let uncanceled_condition = "pool_assignments.canceled_at IS NULL"

module Sql = struct
  let find_request_sql ?(additional_joins = []) ?(count = false) where_fragment =
    let columns = if count then "COUNT(*)" else CCString.concat ", " sql_select_columns in
    Format.asprintf
      {sql|SELECT %s FROM pool_assignments %s %s|sql}
      columns
      (joins :: additional_joins |> CCString.concat "\n")
      where_fragment
  ;;

  let select_public_sql ?(joins = "") where =
    Format.asprintf
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_assignments.uuid), 1, 8), '-',
            SUBSTR(HEX(pool_assignments.uuid), 9, 4), '-',
            SUBSTR(HEX(pool_assignments.uuid), 13, 4), '-',
            SUBSTR(HEX(pool_assignments.uuid), 17, 4), '-',
            SUBSTR(HEX(pool_assignments.uuid), 21)
          )),
          pool_assignments.participated,
          pool_assignments.canceled_at,
          pool_assignments.created_at,
          pool_assignments.updated_at
        FROM
          pool_assignments
        %s
        WHERE
        %s
      |sql}
      joins
      where
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
        WHERE pool_assignments.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> find_request_sql
    |> Pool_common.Repo.Id.t ->! t
  ;;

  let find pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_request id
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Assignment)
  ;;

  let find_closed_request =
    let additional_joins = [ joins_session ] in
    let open Caqti_request.Infix in
    {sql|
     WHERE
      pool_assignments.uuid = UNHEX(REPLACE(?, '-', ''))
    AND
      pool_sessions.closed_at IS NOT NULL
    |sql}
    |> find_request_sql ~additional_joins
    |> Pool_common.Repo.Id.t ->! t
  ;;

  let find_closed pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_closed_request id
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Assignment)
  ;;

  let find_by_session_request ?(where_conditions = []) () =
    let open Caqti_request.Infix in
    let id_fragment =
      {sql|
          pool_assignments.session_uuid = UNHEX(REPLACE(?, '-', ''))
      |sql}
    in
    id_fragment :: where_conditions
    |> CCString.concat " AND "
    |> Format.asprintf "WHERE %s"
    |> find_request_sql
    |> Format.asprintf "%s\n ORDER BY user_users.name, user_users.given_name"
    |> Session.Repo.Id.t ->* t
  ;;

  let find_by_session ?where_conditions pool id =
    Database.collect pool (find_by_session_request ?where_conditions ()) id
  ;;

  let find_multiple_request ids =
    Format.asprintf
      {sql|
        WHERE pool_assignments.session_uuid = UNHEX(REPLACE($1, '-', ''))
        AND pool_assignments.uuid IN ( %s )
      |sql}
      (CCList.mapi (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 2)) ids
       |> CCString.concat ",")
    |> find_request_sql
  ;;

  let find_multiple_by_session pool session_id =
    let open Caqti_request.Infix in
    let open Dynparam in
    function
    | [] -> Lwt.return []
    | ids ->
      let (Pack (pt, pv)) =
        empty
        |> add Session.Repo.Id.t session_id
        |> CCFun.flip (CCList.fold_left (fun dyn id -> dyn |> add Id.t id)) ids
      in
      let request = find_multiple_request ids |> pt ->* t in
      Database.collect pool request pv
  ;;

  let query_by_session ?query pool id =
    let where = "pool_assignments.session_uuid = UNHEX(REPLACE(?, '-', ''))" in
    let dyn = Dynparam.(empty |> add Session.Repo.Id.t id) in
    let select = find_request_sql ?additional_joins:None in
    Query.collect_and_count pool query ~select ~where ~dyn t
  ;;

  let find_deleted_by_session_request () =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_assignments.session_uuid = UNHEX(REPLACE(?, '-', ''))
      AND
        pool_assignments.marked_as_deleted = 1
      |sql}
    |> find_request_sql
    |> Session.Repo.Id.t ->* t
  ;;

  let find_deleted_by_session pool =
    Database.collect pool (find_deleted_by_session_request ())
  ;;

  let count_unsuitable_by_request =
    let open Caqti_request.Infix in
    let base =
      {sql|
        matches_filter = 0
        AND pool_assignments.marked_as_deleted = 0
        AND pool_assignments.canceled_at IS NULL
        AND pool_sessions.canceled_at IS NULL
        AND pool_sessions.closed_at IS NULL
      |sql}
    in
    let count condition =
      Format.asprintf
        "SELECT COUNT(1) FROM pool_assignments %s WHERE %s AND %s"
        joins_session
        base
        condition
      |> Caqti_type.(string ->! int)
    in
    function
    | `Session _ ->
      {sql| pool_assignments.session_uuid = UNHEX(REPLACE(?, '-', '')) |sql} |> count
    | `Experiment _ ->
      Format.asprintf
        {sql| pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', '')) |sql}
      |> count
  ;;

  let count_unsuitable_by pool context =
    let id =
      match context with
      | `Session id -> Session.Id.value id
      | `Experiment id -> Experiment.Id.value id
    in
    Database.find pool (count_unsuitable_by_request context) id
  ;;

  let find_by_contact_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE
        pool_assignments.contact_uuid = UNHEX(REPLACE(?, '-', ''))
      AND
        pool_assignments.marked_as_deleted = 0
    |sql}
    |> find_request_sql
    |> Contact.Repo.Id.t ->* t
  ;;

  let find_by_contact pool = Database.collect pool find_by_contact_request

  let find_upcoming_by_experiment pool id =
    let open Utils.Lwt_result.Infix in
    let* experiment = Experiment.find pool id in
    Session.find_sessions_to_update_matcher pool (`Experiment id)
    >|> Lwt_list.map_s (fun session ->
      let%lwt assignments = find_by_session pool session.Session.id in
      Lwt.return (session, assignments))
    ||> CCPair.make experiment
    |> Lwt_result.ok
  ;;

  let find_upcoming pool =
    let open Utils.Lwt_result.Infix in
    let open Session in
    find_sessions_to_update_matcher pool `Upcoming
    >|> Lwt_list.map_s (fun session ->
      let%lwt assignments = find_by_session pool session.id in
      Lwt.return (session.experiment, (session, assignments)))
    ||> Utils.group_tuples
  ;;

  let find_assigned_contacts_by_experiment_request =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|
        SELECT
          %s
        FROM pool_assignments
          %s
          %s
        WHERE
          pool_sessions.closed_at IS NULL
          AND pool_sessions.canceled_at IS NULL
          AND pool_assignments.marked_as_deleted = 0
          AND pool_assignments.canceled_at IS NULL
          AND pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
    	  GROUP BY
		      pool_assignments.contact_uuid
      |sql}
      (Contact.Repo.sql_select_columns |> CCString.concat ",")
      joins
      joins_session
    |> Experiment.Repo.Entity.Id.t ->* Contact.Repo.t
  ;;

  let find_assigned_contacts_by_experiment pool experiment_id =
    Database.collect pool find_assigned_contacts_by_experiment_request experiment_id
  ;;

  let find_public_by_experiment_and_contact_opt_request =
    let open Caqti_request.Infix in
    let joins =
      {sql|
          LEFT JOIN pool_sessions
          ON
            pool_assignments.session_uuid = pool_sessions.uuid
          AND
            pool_sessions.canceled_at IS NULL
        |sql}
    in
    {sql|
          pool_sessions.experiment_uuid = UNHEX(REPLACE(?, '-', ''))
        AND
          pool_assignments.contact_uuid = UNHEX(REPLACE(?, '-', ''))
        AND
          pool_assignments.marked_as_deleted = 0
      |sql}
    |> select_public_sql ~joins
    |> Caqti_type.t2 Experiment.Repo.Entity.Id.t Contact.Repo.Id.t ->* Public.t
  ;;

  let find_public_by_experiment_and_contact_opt pool experiment_id contact =
    Database.collect
      pool
      find_public_by_experiment_and_contact_opt_request
      (experiment_id, Contact.id contact)
  ;;

  let find_by_contact_and_experiment_request =
    let open Caqti_request.Infix in
    let columns =
      Session.Repo.sql_select_columns @ sql_select_columns |> CCString.concat ", "
    in
    let joins = Format.asprintf "%s\n%s" Session.Repo.joins joins in
    let where =
      {sql|
        pool_sessions.experiment_uuid = UNHEX(REPLACE($1, '-', ''))
        AND pool_assignments.contact_uuid = UNHEX(REPLACE($2, '-', ''))
        AND pool_sessions.canceled_at IS NULL
        AND pool_assignments.marked_as_deleted = 0
      |sql}
    in
    Format.asprintf
      {sql| SELECT %s FROM pool_sessions %s WHERE %s GROUP BY pool_sessions.uuid ORDER BY pool_sessions.start ASC |sql}
      columns
      joins
      where
    |> Caqti_type.t2 Experiment.Repo.Entity.Id.t Contact.Repo.Id.t ->* with_session
  ;;

  let find_by_contact_and_experiment pool experiment_id contact =
    Database.collect
      pool
      find_by_contact_and_experiment_request
      (experiment_id, Contact.id contact)
  ;;

  let find_with_follow_ups_request =
    let open Caqti_request.Infix in
    let additional_joins = [ joins_session ] in
    {sql|
      WHERE
        pool_assignments.marked_as_deleted = 0
      AND(
        pool_assignments.uuid = UNHEX(REPLACE($1, '-', ''))
        OR(
          pool_sessions.follow_up_to = (SELECT session_uuid FROM pool_assignments WHERE pool_assignments.uuid = UNHEX(REPLACE($1, '-', '')))
        AND
          pool_assignments.contact_uuid = (SELECT contact_uuid FROM pool_assignments WHERE pool_assignments.uuid = UNHEX(REPLACE($1, '-', '')))
        ))
    |sql}
    |> find_request_sql ~additional_joins
    |> Id.t ->* t
  ;;

  let find_with_follow_ups pool = Database.collect pool find_with_follow_ups_request

  let find_followups_request =
    let open Caqti_request.Infix in
    let additional_joins = [ joins_session ] in
    {sql|
      WHERE
        pool_sessions.follow_up_to = (SELECT session_uuid FROM pool_assignments WHERE pool_assignments.uuid = UNHEX(REPLACE($1, '-', '')))
      AND
        pool_assignments.contact_uuid = UNHEX(REPLACE($2, '-', ''))
      AND
        pool_assignments.marked_as_deleted = 0
    |sql}
    |> find_request_sql ~additional_joins
    |> Caqti_type.t2 Pool_common.Repo.Id.t Contact.Repo.Id.t ->* t
  ;;

  let find_follow_ups pool m =
    Database.collect pool find_followups_request Entity.(m.id, Contact.id m.contact)
  ;;

  let find_binary_session_id_sql =
    {sql|
      SELECT pool_assignments.session_uuid
      FROM pool_assignments
      WHERE pool_assignments.uuid = ?
    |sql}
  ;;

  let find_session_id_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(session_uuid), 1, 8), '-',
          SUBSTR(HEX(session_uuid), 9, 4), '-',
          SUBSTR(HEX(session_uuid), 13, 4), '-',
          SUBSTR(HEX(session_uuid), 17, 4), '-',
          SUBSTR(HEX(session_uuid), 21)
        ))
      FROM pool_assignments
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Pool_common.Repo.Id.(t ->! t)
  ;;

  let find_session_id pool id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt pool find_session_id_request id
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.Session)
  ;;

  let find_by_contact_to_merge_request =
    let open Caqti_request.Infix in
    {sql|
      WHERE contact_uuid = UNHEX(REPLACE($1, '-', ''))
      AND NOT EXISTS (
        SELECT 1
        FROM pool_assignments AS merge
        WHERE pool_assignments.session_uuid = merge.session_uuid
          AND merge.contact_uuid = UNHEX(REPLACE($2, '-', '')))
    |sql}
    |> find_request_sql
    |> Caqti_type.(t2 Contact.Repo.Id.t Contact.Repo.Id.t) ->* t
  ;;

  let find_by_contact_to_merge pool ~contact ~merged_contact =
    let open Contact in
    Database.collect pool find_by_contact_to_merge_request (id merged_contact, id contact)
  ;;

  let insert_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_assignments (
        uuid,
        session_uuid,
        contact_uuid,
        no_show,
        participated,
        matches_filter,
        canceled_at,
        external_data_id,
        reminder_manually_last_sent_at,
        created_at,
        updated_at
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        UNHEX(REPLACE($2, '-', '')),
        UNHEX(REPLACE($3, '-', '')),
        $4,
        $5,
        $6,
        $7,
        $9,
        $10,
        $11,
        $12
      )
      ON DUPLICATE KEY UPDATE
        marked_as_deleted = 0
    |sql}
    |> Write.t ->. Caqti_type.unit
  ;;

  let insert pool = Database.exec pool insert_request

  let update_request =
    let open Caqti_request.Infix in
    let open Caqti_type in
    {sql|
      UPDATE
        pool_assignments
      SET
        no_show = $2,
        participated = $3,
        matches_filter = $4,
        canceled_at = $5,
        external_data_id = $6,
        reminder_manually_last_sent_at = $7
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> t2
         Id.t
         (t2
            (option bool)
            (t2
               (option bool)
               (t2
                  MatchesFilter.t
                  (t2
                     (option CanceledAt.t)
                     (t2 (option string) (option Pool_common.Repo.Reminder.SentAt.t))))))
       ->. unit
  ;;

  let format_update m =
    let open Entity in
    ( m.id
    , ( m.no_show
      , ( m.participated
        , ( m.matches_filter
          , (m.canceled_at, (m.external_data_id, m.reminder_manually_last_sent_at)) ) ) )
    )
  ;;

  let update pool = format_update %> Database.exec pool update_request

  let marked_as_deleted_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE
        pool_assignments
      SET
        marked_as_deleted = 1
      WHERE
        uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Id.t ->. Caqti_type.unit
  ;;

  let marked_as_deleted pool = Database.exec pool marked_as_deleted_request

  let contact_participation_in_other_assignments_request assignments =
    let ids_sql =
      assignments
      |> CCList.mapi (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 3))
      |> CCString.concat ","
    in
    Format.asprintf
      {sql|
        SELECT
          EXISTS (
            SELECT
              1
            FROM
              pool_assignments
            LEFT JOIN pool_sessions ON pool_assignments.session_uuid = pool_sessions.uuid
            LEFT JOIN pool_experiments ON pool_sessions.experiment_uuid = pool_experiments.uuid
          WHERE
            pool_assignments.uuid NOT IN( %s )
            AND pool_experiments.uuid = UNHEX(REPLACE($1, '-', ''))
            AND pool_assignments.contact_uuid = UNHEX(REPLACE($2, '-', ''))
            AND pool_sessions.closed_at IS NOT NULL
            AND pool_assignments.no_show = 0
            AND pool_assignments.marked_as_deleted = 0
          LIMIT 1)
      |sql}
      ids_sql
  ;;

  let contact_participation_in_other_assignments
        pool
        ~exclude_assignments
        experiment_uuid
        contact_uuid
    =
    if CCList.is_empty exclude_assignments
    then Lwt_result.fail Pool_message.Error.InvalidRequest
    else
      let open Caqti_request.Infix in
      let open Dynparam in
      let open Caqti_type in
      let dyn =
        let init =
          empty
          |> add Experiment.Repo.Entity.Id.t experiment_uuid
          |> add Contact.Repo.Id.t contact_uuid
        in
        CCList.fold_left
          (fun dyn { Entity.id; _ } -> dyn |> add Id.t id)
          init
          exclude_assignments
      in
      let (Pack (pt, pv)) = dyn in
      let request =
        contact_participation_in_other_assignments_request exclude_assignments
        |> pt ->! bool
      in
      Database.find pool request pv |> Lwt.map CCResult.return
  ;;
end

let find = Sql.find
let find_closed = Sql.find_closed

let find_by_session filter pool id =
  match filter with
  | `All -> Sql.find_by_session pool id
  | `NotDeleted -> Sql.find_by_session ~where_conditions:[ not_deleted_condition ] pool id
  | `Uncanceled ->
    Sql.find_by_session
      ~where_conditions:[ not_deleted_condition; uncanceled_condition ]
      pool
      id
  | `Deleted -> Sql.find_deleted_by_session pool id
;;

let find_by_contact pool contact = contact |> Contact.id |> Sql.find_by_contact pool
let find_with_follow_ups = Sql.find_with_follow_ups
let find_follow_ups = Sql.find_follow_ups
let find_session_id = Sql.find_session_id
let insert pool session_id model = model |> Write.of_entity session_id |> Sql.insert pool
let update = Sql.update

let find_public_by_experiment_and_contact_opt =
  Sql.find_public_by_experiment_and_contact_opt
;;

let marked_as_deleted = Sql.marked_as_deleted

let contact_participation_in_other_assignments =
  Sql.contact_participation_in_other_assignments
;;

let find_uncanceled_by_session = find_by_session `Uncanceled
let find_deleted_by_session = find_by_session `Deleted
let find_all_by_session = find_by_session `All
let find_not_deleted_by_session = find_by_session `NotDeleted
let query_by_session = Sql.query_by_session

let enrich_with_customfield_data table_view pool assignments =
  let contact_ids =
    assignments |> CCList.map (fun { Entity.contact; _ } -> Contact.id contact)
  in
  let%lwt custom_fields = Custom_field.find_by_table_view pool table_view in
  let%lwt public_fields =
    Custom_field.find_public_by_contacts_and_view pool true contact_ids table_view
  in
  let rec assign_custom_fields result custom_fields = function
    | [] -> result
    | hd :: tl ->
      let contact_id = hd.Entity.contact |> Contact.id |> Contact.Id.to_common in
      let current, rest =
        CCList.partition_filter_map
          (fun field ->
             field
             |> Custom_field.Public.entity_id
             |> CCOption.map_or ~default:false (Pool_common.Id.equal contact_id)
             |> function
             | true -> `Left field
             | false -> `Right field)
          custom_fields
      in
      let result = result @ [ Entity.{ hd with custom_fields = Some current } ] in
      assign_custom_fields result rest tl
  in
  (assign_custom_fields [] public_fields assignments, custom_fields) |> Lwt.return
;;

let find_with_custom_field_data table_view pool session_id =
  let open Utils.Lwt_result.Infix in
  find_uncanceled_by_session pool session_id
  >|> fun assignments ->
  let contact_ids =
    assignments |> CCList.map (fun { Entity.contact; _ } -> Contact.id contact)
  in
  let%lwt custom_fields = Custom_field.find_by_table_view pool table_view in
  let%lwt public_fields =
    Custom_field.find_public_by_contacts_and_view pool true contact_ids table_view
  in
  let rec assign_custom_fields result custom_fields = function
    | [] -> result
    | hd :: tl ->
      let contact_id = hd.Entity.contact |> Contact.id |> Contact.Id.to_common in
      let current, rest =
        CCList.partition_filter_map
          (fun field ->
             field
             |> Custom_field.Public.entity_id
             |> CCOption.map_or ~default:false (Pool_common.Id.equal contact_id)
             |> function
             | true -> `Left field
             | false -> `Right field)
          custom_fields
      in
      let result = result @ [ Entity.{ hd with custom_fields = Some current } ] in
      assign_custom_fields result rest tl
  in
  (assign_custom_fields [] public_fields assignments, custom_fields) |> Lwt.return
;;
