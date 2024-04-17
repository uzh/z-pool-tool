open CCFun
include Entity

let make_caqti_type caqti_type create value =
  let encode = value %> CCResult.return in
  let decode = create %> CCResult.map_err Pool_message.Error.show in
  Caqti_type.(custom ~encode ~decode caqti_type)
;;

module Label = struct
  include Label

  let t = make_caqti_type Caqti_type.string create value
end

module Url = struct
  include Url

  let t = make_caqti_type Caqti_type.string decrypt encrypt
end

module Disabled = struct
  include Disabled

  let t = make_caqti_type Caqti_type.bool (create %> CCResult.return) value
end

let t =
  let decode (label, (url, (disabled, ()))) = Ok (create ~disabled label url) in
  let encode t : ('a Caqti_encoders.Data.t, string) result =
    Ok Caqti_encoders.Data.[ label t; url t; disabled t ]
  in
  Caqti_encoders.(custom ~encode ~decode Schema.[ Label.t; Url.t; Disabled.t ])
;;

let sql_select_label = "pool_tenant_databases.label"

let sql_database_join_on_label
  ?(join_prefix = "")
  ?(disabled = false)
  label_column
  =
  [%string
    {sql| %{join_prefix} JOIN pool_tenant_databases
      ON pool_tenant_databases.disabled = %{disabled |>CCBool.to_int |> CCInt.to_string}
      AND pool_tenant_databases.label = %{label_column} |sql}]
;;

let sql_select_columns =
  [ sql_select_label
  ; "pool_tenant_databases.url"
  ; "pool_tenant_databases.disabled"
  ]
;;

let joins = ""

module Sql = struct
  open CCFun
  open Caqti_request.Infix
  open Utils.Lwt_result.Infix

  let find_request_sql where_fragment =
    let columns = sql_select_columns |> CCString.concat ", " in
    [%string
      {sql|SELECT %{columns} FROM pool_tenant_databases %{joins} %{where_fragment} |sql}]
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

  let find_all_running_request =
    {sql| WHERE pool_tenant_databases.disabled = false |sql}
    |> find_request_sql
    |> Caqti_type.unit ->* t
  ;;

  let find_all_running label = Service.collect label find_all_running_request ()

  let insert_request =
    {sql|
      INSERT INTO pool_tenant_databases (
        label,
        url,
        disabled
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
        disabled = $4
      WHERE label = $1
    |sql}
    |> Caqti_type.(t2 Label.t t ->. unit)
  ;;

  let update pool database new_database =
    Service.exec pool update_request (database |> label, new_database)
  ;;
end

include Sql
