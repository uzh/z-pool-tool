open CCFun
open Caqti_request.Infix
open Utils.Lwt_result.Infix
open Entity
open Repo_entity

let sql_select_label = "pool_tenant_databases.label"

let sql_database_join_on_label
      ?(join_prefix = "")
      ?(status = `List Status.[ Active; ConnectionIssue; MigrationsPending ])
      label_column
  =
  match status with
  | `All ->
    [%string
      {sql| %{join_prefix} JOIN pool_tenant_databases
      ON pool_tenant_databases.label = %{label_column} |sql}]
  | `List status ->
    [%string
      {sql| %{join_prefix} JOIN pool_tenant_databases
      ON pool_tenant_databases.status IN %{Status.in_fragment status}
      AND pool_tenant_databases.label = %{label_column} |sql}]
;;

let sql_select_columns =
  [ sql_select_label
  ; "pool_tenant_databases.url"
  ; "pool_tenant_databases.status"
  ]
;;

let find_request_sql where_fragment =
  let columns = sql_select_columns |> CCString.concat ", " in
  [%string
    {sql|SELECT %{columns} FROM pool_tenant_databases %{where_fragment} |sql}]
;;

let find_request =
  {sql| WHERE pool_tenant_databases.label = ? |sql}
  |> find_request_sql
  |> Label.t ->! t
;;

let find pool label =
  Service.find_opt pool find_request label
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Label)
;;

let find_all_request = find_request_sql "" |> Caqti_type.unit ->* t
let find_all label = Service.collect label find_all_request ()

let find_all_by_status_request ?(status = []) pt =
  let states =
    CCList.length status |> flip CCList.replicate "?" |> CCString.concat ", "
  in
  [%string {sql| WHERE pool_tenant_databases.status IN (%{states}) |sql}]
  |> find_request_sql
  |> pt ->* t
;;

let find_all_by_status
      ?(status = Status.[ Active; ConnectionIssue; MigrationsPending ])
      label
  =
  let open Dynparam in
  let (Pack (pt, pv)) =
    CCList.fold_left (fun dyn status -> dyn |> add Status.t status) empty status
  in
  Service.collect label (find_all_by_status_request ~status pt) pv
;;

let find_label_by_url_request ?(allowed_status = []) pt =
  let states =
    CCList.length allowed_status
    |> flip CCList.replicate "?"
    |> CCString.concat ", "
  in
  [%string
    {sql|
      WHERE pool_tenant_databases.url = ?
        AND pool_tenant_databases.status IN (%{states})
    |sql}]
  |> find_request_sql
  |> pt ->? Label.t
;;

let find_label_by_url ?(allowed_status = Status.[ Active ]) label url =
  let open Dynparam in
  let init = empty |> add Url.t url in
  let (Pack (pt, pv)) =
    CCList.fold_left
      (fun dyn status -> dyn |> add Status.t status)
      init
      allowed_status
  in
  Service.find_opt label (find_label_by_url_request ~allowed_status pt) pv
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.Url)
;;

let insert_request =
  {sql|
    INSERT INTO pool_tenant_databases (
      label,
      url,
      status
    ) VALUES (
      $1,
      $2,
      $3
    )
  |sql}
  |> t ->. Caqti_type.unit
;;

let insert = flip Service.exec insert_request

let update_request =
  {sql|
    UPDATE pool_tenant_databases
    SET
      label = $2,
      url = $3,
      status = $4
    WHERE label = $1
  |sql}
  |> Caqti_type.(t2 Label.t t ->. unit)
;;

let update pool database new_database =
  Service.exec pool update_request (database |> label, new_database)
;;

let update_status_request =
  {sql|
    UPDATE pool_tenant_databases
    SET status = $2
    WHERE label = $1
  |sql}
  |> Caqti_type.(t2 Label.t Status.t ->. unit)
;;

let update_status pool = curry @@ Service.exec pool update_status_request

let set_migration_pending db_labels =
  match db_labels with
  | [] -> Lwt.return_unit
  | db_labels ->
    let open Dynparam in
    let (Pack (pt, pv)) =
      db_labels
      |> CCList.fold_left
           (fun dyn label -> dyn |> add Caqti_type.string label)
           (empty |> add Status.t Status.MigrationsPending)
    in
    let request =
      Format.asprintf
        {sql|
          UPDATE pool_tenant_databases
          SET status = $1
          WHERE label IN (%s)
        |sql}
        (db_labels
         |> CCList.mapi (fun i _ -> Format.asprintf "$%n" (i + 2))
         |> CCString.concat ",")
      |> pt ->. Caqti_type.unit
    in
    Service.exec root request pv
;;

let database_status_by_label_request =
  {sql|
    SELECT status FROM pool_tenant_databases WHERE label = ?
  |sql}
  |> Label.t ->! Status.t
;;

let database_status_by_label db_label =
  Service.find_opt root database_status_by_label_request db_label
;;
