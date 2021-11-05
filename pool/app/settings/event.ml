open Entity

type event =
  | LanguagesUpdated of Language.t list
  | EmailSuffixesUpdated of EmailSuffix.t list
  | ContactEmailUpdated of ContactEmail.t
  | InactiveUserDisableAfterUpdated of InactiveUser.DisableAfter.t
  | InactiveUserWarningUpdated of InactiveUser.Warning.t

let handle_event pool : event -> unit Lwt.t = function
  | LanguagesUpdated languages ->
    let%lwt _ = Repo.update pool (Value.TenantLanguages languages) in
    Lwt.return_unit
  | EmailSuffixesUpdated email_suffixes ->
    let%lwt _ = Repo.update pool (Value.TenantEmailSuffixes email_suffixes) in
    Lwt.return_unit
  | ContactEmailUpdated contact_email ->
    let%lwt _ = Repo.update pool (Value.TenantContactEmail contact_email) in
    Lwt.return_unit
  | InactiveUserDisableAfterUpdated inactive_user_disable_after ->
    let%lwt _ =
      Repo.update
        pool
        (Value.InactiveUserDisableAfter inactive_user_disable_after)
    in
    Lwt.return_unit
  | InactiveUserWarningUpdated inactive_user_warning ->
    let%lwt _ =
      Repo.update pool (Value.InactiveUserWarning inactive_user_warning)
    in
    Lwt.return_unit
;;

let[@warning "-4"] equal_event event1 event2 =
  match event1, event2 with
  | LanguagesUpdated one, LanguagesUpdated two ->
    Value.equal_tenant_languages one two
  | EmailSuffixesUpdated one, EmailSuffixesUpdated two ->
    Value.equal_tenant_email_suffixes one two
  | ContactEmailUpdated one, ContactEmailUpdated two ->
    Value.equal_tenant_contact_email one two
  | InactiveUserDisableAfterUpdated one, InactiveUserDisableAfterUpdated two ->
    Value.equal_inactive_user_disable_after one two
  | InactiveUserWarningUpdated one, InactiveUserWarningUpdated two ->
    Value.equal_inactive_user_warning one two
  | _ -> false
;;

let pp_event formatter event =
  match event with
  | LanguagesUpdated m -> Value.pp_tenant_languages formatter m
  | EmailSuffixesUpdated m -> Value.pp_tenant_email_suffixes formatter m
  | ContactEmailUpdated m -> Value.pp_tenant_contact_email formatter m
  | InactiveUserDisableAfterUpdated m ->
    Value.pp_inactive_user_disable_after formatter m
  | InactiveUserWarningUpdated m -> Value.pp_inactive_user_warning formatter m
;;
