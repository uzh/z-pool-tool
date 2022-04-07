open Entity
open Default

type event =
  | LanguagesUpdated of Pool_common.Language.t list
  | EmailSuffixesUpdated of EmailSuffix.t list
  | ContactEmailUpdated of ContactEmail.t
  | InactiveUserDisableAfterUpdated of InactiveUser.DisableAfter.t
  | InactiveUserWarningUpdated of InactiveUser.Warning.t
  | TermsAndConditionsUpdated of TermsAndConditions.t list
  | DefaultRestored of default

let handle_event pool : event -> unit Lwt.t = function
  | LanguagesUpdated languages ->
    let%lwt () = Repo.update pool (Value.TenantLanguages languages) in
    Lwt.return_unit
  | EmailSuffixesUpdated email_suffixes ->
    let%lwt () = Repo.update pool (Value.TenantEmailSuffixes email_suffixes) in
    Lwt.return_unit
  | ContactEmailUpdated contact_email ->
    let%lwt () = Repo.update pool (Value.TenantContactEmail contact_email) in
    Lwt.return_unit
  | InactiveUserDisableAfterUpdated inactive_user_disable_after ->
    let%lwt () =
      Repo.update
        pool
        (Value.InactiveUserDisableAfter inactive_user_disable_after)
    in
    Lwt.return_unit
  | InactiveUserWarningUpdated inactive_user_warning ->
    let%lwt () =
      Repo.update pool (Value.InactiveUserWarning inactive_user_warning)
    in
    Lwt.return_unit
  | TermsAndConditionsUpdated terms_and_conditions ->
    let%lwt () =
      Repo.update pool (Value.TermsAndConditions terms_and_conditions)
    in
    Lwt.return_unit
  | DefaultRestored
      { tenant_languages
      ; tenant_email_suffixes
      ; tenant_contact_email
      ; inactive_user_disable_after
      ; inactive_user_warning
      ; terms_and_conditions
      } ->
    let%lwt () =
      [ Languages
      ; EmailSuffixes
      ; ContactEmail
      ; InactiveUserDisableAfter
      ; InactiveUserWarning
      ; TermsAndConditions
      ]
      |> Lwt_list.iter_s (Repo.delete pool)
    in
    let%lwt () =
      Value.
        [ TenantLanguages tenant_languages
        ; TenantEmailSuffixes tenant_email_suffixes
        ; TenantContactEmail tenant_contact_email
        ; InactiveUserDisableAfter inactive_user_disable_after
        ; InactiveUserWarning inactive_user_warning
        ; TermsAndConditions terms_and_conditions
        ]
      |> Lwt_list.iter_s (Repo.insert pool)
    in
    Lwt.return_unit
;;

let equal_event event1 event2 =
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
  | TermsAndConditionsUpdated one, TermsAndConditionsUpdated two ->
    Value.equal_terms_and_conditions one two
  | ( DefaultRestored
        { tenant_languages = one_a
        ; tenant_email_suffixes = one_b
        ; tenant_contact_email = one_c
        ; inactive_user_disable_after = one_d
        ; inactive_user_warning = one_e
        ; terms_and_conditions = one_f
        }
    , DefaultRestored
        { tenant_languages = two_a
        ; tenant_email_suffixes = two_b
        ; tenant_contact_email = two_c
        ; inactive_user_disable_after = two_d
        ; inactive_user_warning = two_e
        ; terms_and_conditions = two_f
        } ) ->
    Value.equal_tenant_languages one_a two_a
    && Value.equal_tenant_email_suffixes one_b two_b
    && Value.equal_tenant_contact_email one_c two_c
    && Value.equal_inactive_user_disable_after one_d two_d
    && Value.equal_inactive_user_warning one_e two_e
    && Value.equal_terms_and_conditions one_f two_f
  | ( ( LanguagesUpdated _
      | EmailSuffixesUpdated _
      | ContactEmailUpdated _
      | InactiveUserDisableAfterUpdated _
      | InactiveUserWarningUpdated _
      | TermsAndConditionsUpdated _
      | DefaultRestored _ )
    , _ ) -> false
;;

let pp_event formatter event =
  match event with
  | LanguagesUpdated m -> Value.pp_tenant_languages formatter m
  | EmailSuffixesUpdated m -> Value.pp_tenant_email_suffixes formatter m
  | ContactEmailUpdated m -> Value.pp_tenant_contact_email formatter m
  | InactiveUserDisableAfterUpdated m ->
    Value.pp_inactive_user_disable_after formatter m
  | InactiveUserWarningUpdated m -> Value.pp_inactive_user_warning formatter m
  | TermsAndConditionsUpdated m -> Value.pp_terms_and_conditions formatter m
  | DefaultRestored
      { tenant_languages = a
      ; tenant_email_suffixes = b
      ; tenant_contact_email = c
      ; inactive_user_disable_after = d
      ; inactive_user_warning = e
      ; terms_and_conditions = f
      } ->
    Value.pp_tenant_languages formatter a;
    Value.pp_tenant_email_suffixes formatter b;
    Value.pp_tenant_contact_email formatter c;
    Value.pp_inactive_user_disable_after formatter d;
    Value.pp_inactive_user_warning formatter e;
    Value.pp_terms_and_conditions formatter f
;;
