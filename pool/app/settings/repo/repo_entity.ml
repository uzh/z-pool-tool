open Entity

let encode_key_value =
  let open Value in
  function
  | TenantLanguages value ->
    ( setting_key_to_yojson Languages |> Yojson.Safe.to_string
    , value |> Value.tenant_languages_to_yojson |> Yojson.Safe.to_string )
  | TenantEmailSuffixes value ->
    ( setting_key_to_yojson EmailSuffixes |> Yojson.Safe.to_string
    , value |> Value.tenant_email_suffixes_to_yojson |> Yojson.Safe.to_string )
  | TenantContactEmail value ->
    ( setting_key_to_yojson ContactEmail |> Yojson.Safe.to_string
    , value |> Value.tenant_contact_email_to_yojson |> Yojson.Safe.to_string )
  | InactiveUserDisableAfter value ->
    ( setting_key_to_yojson InactiveUserDisableAfter |> Yojson.Safe.to_string
    , value
      |> Value.inactive_user_disable_after_to_yojson
      |> Yojson.Safe.to_string )
  | InactiveUserWarning value ->
    ( setting_key_to_yojson InactiveUserWarning |> Yojson.Safe.to_string
    , value |> Value.inactive_user_warning_to_yojson |> Yojson.Safe.to_string )
;;

let t =
  let encode m =
    let key, value = encode_key_value m.value in
    Ok (key, value, m.created_at, m.updated_at)
  in
  let decode (key, value, created_at, updated_at) =
    let open CCResult in
    let* key = key |> Yojson.Safe.from_string |> setting_key_of_yojson in
    let* value =
      let value = value |> Yojson.Safe.from_string in
      match key with
      | Languages ->
        value
        |> Value.tenant_languages_of_yojson
        |> CCResult.map (fun v -> Value.TenantLanguages v)
      | EmailSuffixes ->
        value
        |> Value.tenant_email_suffixes_of_yojson
        |> CCResult.map (fun v -> Value.TenantEmailSuffixes v)
      | ContactEmail ->
        value
        |> Value.tenant_contact_email_of_yojson
        |> CCResult.map (fun v -> Value.TenantContactEmail v)
      | InactiveUserDisableAfter ->
        value
        |> Value.inactive_user_disable_after_of_yojson
        |> CCResult.map (fun v -> Value.InactiveUserDisableAfter v)
      | InactiveUserWarning ->
        value
        |> Value.inactive_user_warning_of_yojson
        |> CCResult.map (fun v -> Value.InactiveUserWarning v)
    in
    Ok { value; created_at; updated_at }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup4
         string
         string
         Pool_common.Repo.CreatedAt.t
         Pool_common.Repo.UpdatedAt.t))
;;

module Write = struct
  include Write

  let t =
    let encode m =
      let key, value = encode_key_value m.Write.value in
      Ok (value, key)
    in
    let decode _ = failwith "Write only model" in
    Caqti_type.(custom ~encode ~decode (tup2 string string))
  ;;
end
