open Entity
open Database.Caqti_encoders
open Caqti_request.Infix

module RepoEntity = struct
  module ApiKey = struct
    include ApiKey

    let t =
      let open Utils.Crypto.String in
      Pool_common.Repo.make_caqti_type
        Caqti_type.string
        (fun s ->
           s
           |> decrypt_from_string
           |> CCResult.map_err (fun _ -> Pool_message.(Error.Decode Field.ApiKey)))
        encrypt_to_string
    ;;
  end

  let t =
    let decode (id, (api_key, (sender, (created_at, (updated_at, ()))))) =
      Ok { id; api_key; sender; created_at; updated_at }
    in
    let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
    let open Schema in
    custom
      ~encode
      ~decode
      Caqti_type.
        [ Pool_common.Repo.Id.t
        ; ApiKey.t
        ; string
        ; Pool_common.Repo.CreatedAt.t
        ; Pool_common.Repo.UpdatedAt.t
        ]
  ;;

  let write =
    let decode _ = Pool_common.Utils.failwith Pool_message.Error.WriteOnlyModel in
    let encode m : ('a Data.t, string) result = Ok Data.[ m.id; m.api_key; m.sender ] in
    let open Schema in
    custom ~encode ~decode Caqti_type.[ Pool_common.Repo.Id.t; ApiKey.t; string ]
  ;;
end

module Cache = struct
  open Hashtbl

  let tbl : (Database.Label.t, Entity.t) t = create 5
  let find = find_opt tbl
  let add = replace tbl
  let clear () = clear tbl
end

let sql_select_columns =
  [ Entity.Id.sql_select_fragment ~field:"pool_gtx_api_keys.uuid"
  ; "pool_gtx_api_keys.api_key"
  ; "pool_gtx_api_keys.sender_name"
  ; "pool_gtx_api_keys.created_at"
  ; "pool_gtx_api_keys.updated_at"
  ]
;;

(* TODO: Do I need ID? *)
let find_opt_request =
  sql_select_columns
  |> CCString.concat ", "
  |> Format.asprintf {sql| SELECT %s FROM pool_gtx_api_keys |sql}
  |> Caqti_type.unit ->? RepoEntity.t
;;

let insert_request =
  {sql|
    INSERT INTO pool_gtx_api_keys (
      uuid,
      api_key,
      sender_name
    ) VALUES (
      UNHEX(REPLACE($1, '-', '')),
      $2,
      $3
    )
  |sql}
  |> RepoEntity.write ->. Caqti_type.unit
;;

let update_request =
  let open Caqti_request.Infix in
  {sql|
    UPDATE pool_gtx_api_keys
    SET
      api_key = $2,
      sender_name = $3
    WHERE
      uuid = UNHEX(REPLACE($1, '-', ''))
  |sql}
  |> RepoEntity.write ->. Caqti_type.unit
;;

let find_opt pool =
  let open Utils.Lwt_result.Infix in
  Cache.find pool
  |> function
  | Some config -> Lwt.return_some config
  | None ->
    Database.find_opt pool find_opt_request ()
    ||> CCOption.map (fun config ->
      Cache.add pool config;
      config)
;;

let find_exn pool =
  let open Utils.Lwt_result.Infix in
  find_opt pool
  ||> CCOption.to_result Pool_message.(Error.NotFound Field.GtxApiKey)
  ||> Pool_common.Utils.get_or_failwith
;;

let insert pool = Database.exec pool insert_request
let update pool = Database.exec pool update_request
