open CCFun.Infix
open Entity
module Database = Database

let make_caqti ~encode ~decode =
  let encode = encode %> Yojson.Safe.to_string %> CCResult.return in
  let decode = Yojson.Safe.from_string %> decode %> CCResult.return in
  Caqti_type.(custom ~encode ~decode Caqti_type.string)
;;

let key_to_string = Entity.Key.yojson_of_t %> Yojson.Safe.to_string

module Sql = struct
  let select_from_settings_sql =
    {sql|
      SELECT
         value
      FROM
        pool_system_settings
      WHERE
        settings_key = ?
    |sql}
  ;;

  let find_request out_type =
    let open Caqti_request.Infix in
    select_from_settings_sql |> Caqti_type.string ->! out_type
  ;;

  let find pool out_type key =
    Database.find pool (find_request out_type) (key_to_string key)
  ;;

  let update_sql =
    {sql|
      UPDATE
        pool_system_settings
      SET
        value = ?
      WHERE
        settings_key = ?
    |sql}
  ;;

  let exec_update pool caqti_type key value =
    let open Caqti_request.Infix in
    let request = update_sql |> Caqti_type.(t2 caqti_type string ->. unit) in
    Database.exec pool request (value, key_to_string key)
  ;;

  let find_setting_id pool key =
    let request =
      let open Caqti_request.Infix in
      [%string
        {sql|
        SELECT %{Pool_model.Base.Id.sql_select_fragment ~field:"uuid"}
        FROM pool_system_settings
        WHERE settings_key = ?
      |sql}]
      |> Caqti_type.(string ->! Pool_common.Repo.Id.t)
    in
    key |> Entity.Key.yojson_of_t |> Yojson.Safe.to_string |> Database.find pool request
  ;;
end

let insert_contact_email pool email =
  let open Caqti_request.Infix in
  let open Entity.ContactEmail in
  let caqti_type = make_caqti ~encode:yojson_of_t ~decode:t_of_yojson in
  let request =
    {sql|
      INSERT INTO pool_system_settings (
        uuid,
        settings_key,
        value
      ) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        ?,
        ?
      )
    |sql}
    |> Caqti_type.(t2 string caqti_type ->. unit)
  in
  Database.exec pool request (key_to_string key, email)
;;

module type SettingRepoSig = sig
  type t

  val key : Entity.Key.t
  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

let id_by_key = Sql.find_setting_id

module SettingRepo (T : SettingRepoSig) = struct
  include T

  module Changelog = Changelog.T (struct
      include Changelog.DefaultSettings
      include T

      let model = Pool_message.Field.Setting
    end)

  let caqti_type = make_caqti ~encode:yojson_of_t ~decode:t_of_yojson
  let find pool = Sql.find pool caqti_type key
  let find_id pool = Sql.find_setting_id pool key

  let create_changelog ?user_uuid pool after =
    let%lwt before = find pool in
    let%lwt entity_uuid = Sql.find_setting_id pool key in
    Changelog.insert pool ?user_uuid ~entity_uuid ~before ~after ()
  ;;

  let update ?user_uuid pool m =
    let%lwt () = create_changelog ?user_uuid pool m in
    Sql.exec_update pool caqti_type key m
  ;;
end

module DefaultReminderLeadTime = SettingRepo (EmailReminderLeadTime)
module DefaultTextMsgReminderLeadTime = SettingRepo (TextMsgReminderLeadTime)
module TenantLanguages = SettingRepo (TenantLanguages)
module TenantEmailSuffixes = SettingRepo (EmailSuffixes)
module TenantContactEmail = SettingRepo (ContactEmail)
module InactiveUserDisableAfter = SettingRepo (InactiveUser.DisableAfter)
module InactiveUserWarning = SettingRepo (InactiveUser.Warning)
module InactiveUserServiceDisabled = SettingRepo (InactiveUser.ServiceDisabled)
module TriggerProfileUpdateAfter = SettingRepo (TriggerProfileUpdateAfter)
module UserImportFirstReminder = SettingRepo (UserImportReminder.FirstReminderAfter)
module UserImportSecondReminder = SettingRepo (UserImportReminder.SecondReminderAfter)

module PageScripts = struct
  open Entity.PageScript

  module Cache = struct
    open Hashtbl

    let tbl : (Database.Label.t, page_scripts) t = create 5
    let find = find_opt tbl
    let add database_label = replace tbl database_label
    let update = add
    let clear () = clear tbl
  end

  let create_changelog ?user_uuid pool location after =
    let current_request =
      let open Caqti_request.Infix in
      [%string
        {sql|
          SELECT
            %{Pool_model.Base.Id.sql_select_fragment ~field:"uuid"},
            script
          FROM
            pool_tenant_page_scripts
          WHERE location = ?
        |sql}]
      |> Caqti_type.(string ->! t2 Pool_common.Repo.Id.t (option string))
    in
    let%lwt current = Database.find_opt pool current_request (show_location location) in
    match current with
    | None -> Lwt.return ()
    | Some (entity_uuid, before) ->
      let default = CCOption.value ~default:"" in
      Entity.PageScriptChangelog.insert
        pool
        ?user_uuid
        ~entity_uuid
        ~before:(default before)
        ~after:(default after)
        ()
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      INSERT INTO pool_tenant_page_scripts (
        uuid,
        location,
        script
      ) VALUES (
        UNHEX(REPLACE(UUID(), '-', '')),
        $1,
        $2
      ) ON DUPLICATE KEY UPDATE
        script = VALUES(script)
    |sql}
    |> Caqti_type.(t2 string string ->. Caqti_type.unit)
  ;;

  let clear_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_tenant_page_scripts
      SET script = NULL
      WHERE location = ?
    |sql}
    |> Caqti_type.(string ->. unit)
  ;;

  let update ?user_uuid pool (script, location) =
    let%lwt () = create_changelog ?user_uuid pool location script in
    match script with
    | None -> Database.exec pool clear_request (show_location location)
    | Some script -> Database.exec pool update_request (show_location location, script)
  ;;

  let find_id pool location =
    let open Caqti_request.Infix in
    let request =
      [%string
        {sql|
          SELECT
            %{Pool_model.Base.Id.sql_select_fragment ~field:"uuid"}
          FROM
            pool_tenant_page_scripts
          WHERE location = ?
        |sql}]
      |> Caqti_type.(string ->! Pool_common.Repo.Id.t)
    in
    Database.find pool request (show_location location)
  ;;

  let find_request =
    let open Caqti_request.Infix in
    {sql|
      SELECT
        script
      FROM
        pool_tenant_page_scripts
      WHERE location = ?
      AND script IS NOT NULL  
    |sql}
    |> Caqti_type.(string ->? string)
  ;;

  let find pool location = Database.find_opt pool find_request (show_location location)

  let find pool =
    match Cache.find pool with
    | Some scripts -> Lwt.return scripts
    | None ->
      let%lwt head = find pool Head in
      let%lwt body = find pool Body in
      let scripts = { head; body } in
      Cache.add pool scripts;
      Lwt.return scripts
  ;;
end
