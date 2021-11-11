open Entity

let encode_key_value value =
  (let open Value in
  match value with
  | TenantLanguages v ->
    setting_key_to_yojson Languages, tenant_languages_to_yojson v
  | TenantEmailSuffixes v ->
    setting_key_to_yojson EmailSuffixes, tenant_email_suffixes_to_yojson v
  | TenantContactEmail v ->
    setting_key_to_yojson ContactEmail, tenant_contact_email_to_yojson v
  | InactiveUserDisableAfter v ->
    ( setting_key_to_yojson InactiveUserDisableAfter
    , inactive_user_disable_after_to_yojson v )
  | InactiveUserWarning v ->
    setting_key_to_yojson InactiveUserWarning, inactive_user_warning_to_yojson v
  | TermsAndConditions v ->
    setting_key_to_yojson TermsAndConditions, terms_and_conditions_to_yojson v)
  |> fun (m, k) -> m |> Yojson.Safe.to_string, k |> Yojson.Safe.to_string
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
      let open CCResult.Infix in
      let open Value in
      let value = value |> Yojson.Safe.from_string in
      match key with
      | Languages ->
        value |> Value.tenant_languages_of_yojson >|= tenantlanguages
      | EmailSuffixes ->
        value |> Value.tenant_email_suffixes_of_yojson >|= tenantemailsuffixes
      | ContactEmail ->
        value |> Value.tenant_contact_email_of_yojson >|= tenantcontactemail
      | InactiveUserDisableAfter ->
        value
        |> Value.inactive_user_disable_after_of_yojson
        >|= inactiveuserdisableafter
      | InactiveUserWarning ->
        value |> Value.inactive_user_warning_of_yojson >|= inactiveuserwarning
      | TermsAndConditions ->
        value |> Value.terms_and_conditions_of_yojson >|= termsandconditions
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
