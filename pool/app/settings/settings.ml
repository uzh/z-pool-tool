include Entity
include Event

let[@warning "-4"] find_languages pool () =
  let open Utils.Lwt_result.Infix in
  Repo.find_languages pool ()
  >>= fun setting ->
  let open Value in
  match setting.value with
  | TenantLanguages value -> Lwt.return_ok value
  | _ -> Lwt.return_error "Cannot retrieve pool languages"
;;

let[@warning "-4"] find_email_suffixes pool () =
  let open Utils.Lwt_result.Infix in
  Repo.find_email_suffixes pool ()
  >>= fun setting ->
  let open Value in
  match setting.value with
  | TenantEmailSuffixes value -> Lwt.return_ok value
  | _ -> Lwt.return_error "Cannot retrieve pool email suffixes"
;;

let[@warning "-4"] find_contact_email pool () =
  let open Utils.Lwt_result.Infix in
  Repo.find_contact_email pool ()
  >>= fun setting ->
  let open Value in
  match setting.value with
  | TenantContactEmail value -> Lwt.return_ok value
  | _ -> Lwt.return_error "Cannot retrieve pool contact email"
;;

let[@warning "-4"] find_inactive_user_disable_after pool () =
  let open Utils.Lwt_result.Infix in
  Repo.find_inactive_user_disable_after pool ()
  >>= fun setting ->
  let open Value in
  match setting.value with
  | InactiveUserDisableAfter value -> Lwt.return_ok value
  | _ -> Lwt.return_error "Cannot retrieve inactive user disable after"
;;

let[@warning "-4"] find_inactive_user_warning pool () =
  let open Utils.Lwt_result.Infix in
  Repo.find_inactive_user_warning pool ()
  >>= fun setting ->
  let open Value in
  match setting.value with
  | InactiveUserWarning value -> Lwt.return_ok value
  | _ -> Lwt.return_error "Cannot retrieve inactive user warning"
;;

let[@warning "-4"] find_terms_and_conditions pool () =
  let open Utils.Lwt_result.Infix in
  Repo.find_terms_and_conditions pool ()
  >>= fun setting ->
  let open Value in
  match setting.value with
  | TermsAndConditions value -> Lwt.return_ok value
  | _ -> Lwt.return_error "Cannot retrieve terms and conditions"
;;

let terms_and_conditions_last_updated pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_terms_and_conditions pool () >|= fun setting -> setting.updated_at
;;
