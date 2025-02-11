open Sexplib.Conv
open Ppx_yojson_conv_lib.Yojson_conv

let go = Utils.ppx_printer

let custom _ fmt t =
  let _, name = t in
  Format.pp_print_string fmt name
;;

let nested name show fmt t =
  Format.pp_print_string fmt (Format.asprintf "%s[%s]" name (show t))
;;

type t =
  | Action [@name "action"] [@printer go "action"]
  | Active [@name "active"] [@printer go "active"]
  | ActiveContactsCount [@name "active_contacts_count"]
  [@printer go "active_contacts_count"]
  | Actor [@name "actor"] [@printer go "actor"]
  | Admin [@name "admin"] [@printer go "admin"]
  | AdminComment [@name "admin_comment"] [@printer go "admin_comment"]
  | AdminHint [@name "admin_hint"] [@printer go "admin_hint"]
  | AdminInput [@name "admin_input"] [@printer go "admin_input"]
  | AdminInputOnly [@name "admin_input_only"] [@printer go "admin_input_only"]
  | AdminViewOnly [@name "admin_view_only"] [@printer go "admin_view_only"]
  | AllowUninvitedSignup [@name "allow_uninvited_signup"]
  [@printer go "allow_uninvited_signup"]
  | Announcement [@name "announcement"] [@printer go "announcement"]
  | Answer [@name "answer"] [@printer go "answer"]
  | ApiKey [@name "api_key"] [@printer go "api_key"]
  | AreaCode [@name "area_code"] [@printer go "area_code"]
  | Argument [@name "argument"] [@printer go "argument"]
  | AssetId [@name "asset_id"] [@printer go "asset_id"]
  | AssignableRole [@name "assignable_role"] [@printer go "assignable_role"]
  | Assignment [@name "assignment"] [@printer go "assignment"]
  | AssignmentCount [@name "assignment_count"] [@printer go "assignment_count"]
  | Assignments [@name "assignments"] [@printer go "assignments"]
  | AssignmentsCreated [@name "assignments_created"] [@printer go "assignments_created"]
  | AssignmentWithoutSession [@name "assignment_without_session"]
  [@printer go "assignment_without_session"]
  | Assistants [@name "assistants"] [@printer go "assistants"]
  | AvailableLanguages [@name "available_languages"] [@printer go "available_languages"]
  | Building [@name "building"] [@printer go "building"]
  | CallbackUrl [@name "callbackUrl"] [@printer go "callbackUrl"]
  | CanceledAt [@name "canceled_at"] [@printer go "canceled_at"]
  | CellPhone [@name "cell_phone"] [@printer go "cell_phone"]
  | Changes [@name "changes"] [@printer go "changes"]
  | Chronological [@name "chronological"] [@printer go "chronological"]
  | City [@name "city"] [@printer go "city"]
  | ClosedAt [@name "closed_at"] [@printer go "closed_at"]
  | Code [@name "code"] [@printer go "code"]
  | Confirmed [@name "confirmed"] [@printer go "confirmed"]
  | ConfirmedAt [@name "confirmed_at"] [@printer go "confirmed_at"]
  | Contact [@name "contact"] [@printer go "contact"]
  | ContactCount [@name "contact_count"] [@printer go "contact_count"]
  | ContactEmail [@name "contact_email"] [@printer go "contact_email"]
  | ContactLanguage [@name "contact_language"] [@printer go "contact_language"]
  | ContactPerson [@name "contact_person"] [@printer go "contact_person"]
  | Contacts [@name "contacts"] [@printer go "contacts"]
  | Context [@name "context"] [@printer go "context"]
  | CostCenter [@name "cost_center"] [@printer go "cost_center"]
  | Count [@name "count"] [@printer go "count"]
  | CreatedAt [@name "created_at"] [@printer go "created_at"]
  | CurrentPassword [@name "current_password"] [@printer go "current_password"]
  | CustomField [@name "custom_field"] [@printer go "custom_field"]
  | CustomFieldAnswer [@name "custom_field_answer"] [@printer go "custom_field_answer"]
  | CustomFieldGroup [@name "custom_field_group"] [@printer go "custom_field_group"]
  | CustomFieldGroups [@name "custom_field_groups"] [@printer go "custom_field_groups"]
  | CustomFieldOption [@name "custom_field_option"] [@printer go "custom_field_option"]
  | CustomFieldOptions [@name "custom_field_options"] [@printer go "custom_field_options"]
  | CustomFields [@name "custom_fields"] [@printer go "custom_fields"]
  | CustomHtmx of (string * string) [@name "custom"] [@printer custom "custom"]
  | Database [@name "database"] [@printer go "database"]
  | DatabaseLabel [@name "database_label"] [@printer go "database_label"]
  | DatabaseUrl [@name "database_url"] [@printer go "database_url"]
  | Date [@name "date"] [@printer go "date"]
  | DateTime [@name "date_time"] [@printer go "date_time"]
  | DefaultLanguage [@name "default_language"] [@printer go "default_language"]
  | DefaultSmtpServer [@name "default"] [@printer go "default"]
  | DeliveryReport [@name "delivery_report"] [@printer go "delivery_report"]
  | Description [@name "description"] [@printer go "description"]
  | DirectRegistrationDisabled [@name "direct_registration_disabled"]
  [@printer go "direct_registration_disabled"]
  | Disabled [@name "disabled"] [@printer go "disabled"]
  | Distribution [@name "distribution"] [@printer go "distribution"]
  | DistributionField [@name "distribution_field"] [@printer go "distribution_field"]
  | Duration [@name "duration"] [@printer go "duration"]
  | Duplicate [@name "duplicate"] [@printer go "duplicate"]
  | DuplicateWeighting [@name "duplicate_weighting"] [@printer go "duplicate_weighting"]
  | Email [@name "email"] [@printer go "email"]
  | EmailAddress [@name "email_address"] [@printer go "email_address"]
  | EmailAddressUnverified [@name "email_address_unverified"]
  [@printer go "email_address_unverified"]
  | EmailAddressVerified [@name "email_address_verified"]
  [@printer go "email_address_verified"]
  | EmailLeadTime [@name "email_lead_time"] [@printer go "email_lead_time"]
  | EmailLogo [@name "email_logo"] [@printer go "email_logo"]
  | EmailRemindersSentAt [@name "email_reminders_sent_at"]
  [@printer go "email_reminders_sent_at"]
  | EmailsSent [@name "emails_sent"] [@printer go "emails_sent"]
  | EmailSubject [@name "email_subject"] [@printer go "email_subject"]
  | EmailSuffix [@name "email_suffix"] [@printer go "email_suffix"]
  | EmailText [@name "email_text"] [@printer go "email_text"]
  | End [@name "end"] [@printer go "end"]
  | ExampleValue [@name "example_value"] [@printer go "example_value"]
  | Exclude [@name "exclude"] [@printer go "exclude"]
  | ExcludeRolesOf [@name "exclude_roles_of"] [@printer go "exclude_roles_of"]
  | Experiment [@name "experiment"] [@printer go "experiment"]
  | ExperimentCount [@name "experiment_count"] [@printer go "experiment_count"]
  | ExperimentEmailReminderLeadTime [@name "experiment_email_reminder_lead_time"]
  [@printer go "experiment_email_reminder_lead_time"]
  | Experimenter [@name "experimenter"] [@printer go "experimenter"]
  | Experiments [@name "experiments"] [@printer go "experiments"]
  | ExperimentTextMessageReminderLeadTime [@name "experiment_text_message_lead_time"]
  [@printer go "experiment_text_message_lead_time"]
  | ExperimentType [@name "experiment_type"] [@printer go "experiment_type"]
  | ExpiresAt [@name "expires_at"] [@printer go "expires_at"]
  | ExternalDataId [@name "external_data_id"] [@printer go "external_data_id"]
  | ExternalDataIdAbbr [@name "edi"] [@printer go "edi"]
  | ExternalDataRequired [@name "external_data_required"]
  [@printer go "external_data_required"]
  | Failed [@name "failed"] [@printer go "failed"]
  | FallbackToEmail [@name "fallback_to_email"] [@printer go "fallback_to_email"]
  | FieldType [@name "field_type"] [@printer go "field_type"]
  | File [@name "file"] [@printer go "file"]
  | FileMapping [@name "file_mapping"] [@printer go "file_mapping"]
  | FileMimeType [@name "file_mime_type"] [@printer go "file_mime_type"]
  | Filename [@name "filename"] [@printer go "filename"]
  | Filesize [@name "filesize"] [@printer go "filesize"]
  | Filter [@name "filter"] [@printer go "filter"]
  | Firstname [@name "firstname"] [@printer go "firstname"]
  | FirstReminder [@name "first_reminder"] [@printer go "first_reminder"]
  | FollowUpSession [@name "follow_up_session"] [@printer go "follow_up_session"]
  | GtxApiKey [@name "gtx_api_key"] [@printer go "gtx_api_key"]
  | GtxSender [@name "gtx_sender"] [@printer go "gtx_sender"]
  | HideCanceled [@name "hide_canceled"] [@printer go "hide_canceled"]
  | HideClosed [@name "hide_closed"] [@printer go "hide_closed"]
  | HideInactive [@name "hide_inactive"] [@printer go "hide_inactive"]
  | HideIgnored [@name "hide_ignored"] [@printer go "hide_ignored"]
  | HideMakedAsDeleted [@name "hide_marked_as_deleted"]
  [@printer go "hide_marked_as_deleted"]
  | HidePast [@name "hide_past"] [@printer go "hide_past"]
  | HidePaused [@name "hide_paused"] [@printer go "hide_paused"]
  | HideUnverified [@name "hide_unverified"] [@printer go "hide_unverified"]
  | Hint [@name "hint"] [@printer go "hint"]
  | History [@name "history"] [@printer go "history"]
  | Host [@name "host"] [@printer go "host"]
  | I18n [@name "i18n"] [@printer go "i18n"]
  | Icon [@name "icon"] [@printer go "icon"]
  | Id [@name "id"] [@printer go "id"]
  | ImportPending [@name "import_pending"] [@printer go "import_pending"]
  | Inactive [@name "inactive"] [@printer go "inactive"]
  | InactiveUserDisableAfter [@name "inactive_user_disable_after"]
  [@printer go "inactive_user_disable_after"]
  | InactiveUserWarning [@name "inactive_user_warning"]
  [@printer go "inactive_user_warning"]
  | InactiveUserDisableService [@name "inactive_user_disable_service"]
  [@printer go "inactive_user_disable_service"]
  | Input [@name "input"] [@printer go "input"]
  | Institution [@name "institution"] [@printer go "institution"]
  | InternalDescription [@name "internal_description"]
  [@printer go "internal_description"]
  | Interval [@name "interval"] [@printer go "interval"]
  | Invitation [@name "invitation"] [@printer go "invitation"]
  | InvitationCount [@name "invitation_count"] [@printer go "invitation_count"]
  | InvitationResetAt [@name "invitation_reset_at"] [@printer go "invitation_reset_at"]
  | Invitations [@name "invitations"] [@printer go "invitations"]
  | InvitationsSent [@name "invitations_sent"] [@printer go "invitations_sent"]
  | InvitationSubject [@name "invitation_subject"] [@printer go "invitation_subject"]
  | InvitationText [@name "invitation_text"] [@printer go "invitation_text"]
  | IsAdmin [@name "is_admin"] [@printer go "is_admin"]
  | Key [@name "key"] [@printer go "key"]
  | Label [@name "label"] [@printer go "label"]
  | Language [@name "language"] [@printer go "language"]
  | LanguageDe [@name "DE"] [@printer go "DE"]
  | LanguageEn [@name "EN"] [@printer go "EN"]
  | LastError [@name "last_error"] [@printer go "last_error"]
  | LastErrorAt [@name "last_error_at"] [@printer go "last_error_at"]
  | LastManuallyRemindedAt [@name "last_manually_reminded_at"]
  [@printer go "last_manually_reminded_at"]
  | Lastname [@name "lastname"] [@printer go "lastname"]
  | LastRemindedAt [@name "last_reminded_at"] [@printer go "reminded_at"]
  | LastRun [@name "last_run"] [@printer go "last_run"]
  | LastRunAt [@name "last_run_at"] [@printer go "last_run_at"]
  | LeadTime [@name "lead_time"] [@printer go "lead_time"]
  | Limit [@name "limit"] [@printer go "limit"]
  | Link [@name "link"] [@printer go "link"]
  | Location [@name "location"] [@printer go "location"]
  | Locations [@name "locations"] [@printer go "locations"]
  | LoginCount [@name "login_count"] [@printer go "login_count"]
  | LogoType [@name "logo_type"] [@printer go "logo_type"]
  | Mailing [@name "mailing"] [@printer go "mailing"]
  | MainSession [@name "main_session"] [@printer go "main_session"]
  | MarkedAsDeleted [@name "marked_as_deleted"] [@printer go "marked_as_deleted"]
  | MatchingFilterCount [@name "matching_filter_count"]
  [@printer go "matching_filter_count"]
  | MaxParticipants [@name "max_participants"] [@printer go "max_participants"]
  | MaxTries [@name "max_tries"] [@printer go "max_tries"]
  | Message [@name "message"] [@printer go "message"]
  | MessageChannel [@name "message_channel"] [@printer go "message_channel"]
  | MessageTemplate [@name "message_template"] [@printer go "message_template"]
  | MessageTemplates [@name "message_templates"] [@printer go "message_templates"]
  | MinParticipants [@name "min_participants"] [@printer go "min_participants"]
  | Model [@name "model"] [@printer go "model"]
  | Name [@name "name"] [@printer go "name"]
  | NewPassword [@name "new_password"] [@printer go "new_password"]
  | NextRunAt [@name "next_run_at"] [@printer go "next_run_at"]
  | NoShow [@name "no_show"] [@printer go "no_show"]
  | NoShowAbr [@name "no_show_abr"] [@printer go "no_show_abr"]
  | NoShowCount [@name "no_show_count"] [@printer go "no_show_count"]
  | NotifiedAt [@name "notified_at"] [@printer go "notified_at"]
  | NotifyContact [@name "notify_contact"] [@printer go "notify_contact"]
  | NotifyVia [@name "notify_via"] [@printer go "notify_via"]
  | NotMatchingFilterCount [@name "not_matching_filter_count"]
  [@printer go "not_matching_filter_count"]
  | Offset [@name "offset"] [@printer go "offset"]
  | OnlineExperiment [@name "online_experiment"] [@printer go "online_experiment"]
  | Operator [@name "operator"] [@printer go "operator"]
  | Operators [@name "operators"] [@printer go "operators"]
  | Order [@name "order"] [@printer go "order"]
  | OrganisationalUnit [@name "organisational_unit"] [@printer go "organisational_unit"]
  | Overbook [@name "overbook"] [@printer go "overbook"]
  | OverriddenValue [@name "overridden_value"] [@printer go "overridden_value"]
  | Override [@name "override"] [@printer go "override"]
  | Page [@name "page"] [@printer go "page"]
  | PageCount [@name "page_count"] [@printer go "page_count"]
  | PageScriptsHead [@name "page_scripts_head"] [@printer go "page_scripts_head"]
  | PageScriptsBody [@name "page_scripts_body"] [@printer go "page_scripts_body"]
  | Participant [@name "participant"] [@printer go "participant"]
  | ParticipantCount [@name "participant_count"] [@printer go "participant_count"]
  | Participants [@name "participants"] [@printer go "participants"]
  | Participated [@name "participated"] [@printer go "participated"]
  | ParticipatedAbr [@name "participated_abr"] [@printer go "participated_abr"]
  | ParticipationTag [@name "participation_tag"] [@printer go "participation_tag"]
  | ParticipationTags [@name "participation_tags"] [@printer go "participation_tags"]
  | PartnerLogos [@name "partner_logos"] [@printer go "partner_logos"]
  | Password [@name "password"] [@printer go "password"]
  | PasswordConfirmation [@name "password_confirmation"]
  [@printer go "password_confirmation"]
  | Paused [@name "paused"] [@printer go "paused"]
  | PendingContactImports [@name "pending_contact_imports"]
  [@printer go "pending_contact_imports"]
  | Period [@name "period"] [@printer go "period"]
  | Permission [@name "permission"] [@printer go "permission"]
  | PermissionOn of string * string [@name "permission_on"] [@printer go "permission_on"]
  | Placeholder [@name "placeholder"] [@printer go "placeholder"]
  | PlainText [@name "plain_text"] [@printer go "plain_text"]
  | Predicate [@name "predicate"] [@printer go "predicate"]
  | Profile [@name "profile"] [@printer go "profile"]
  | PromptOnRegistration [@name "prompt_on_registration"]
  [@printer go "prompt_on_registration"]
  | PublicDescription [@name "public_description"] [@printer go "public_description"]
  | PublicTitle [@name "public_title"] [@printer go "public_title"]
  | PublishedAt [@name "published_at"] [@printer go "published_at"]
  | Query [@name "query"] [@printer go "query"]
  | Queue [@name "queue"] [@printer go "queue"]
  | RandomOrder [@name "random_order"] [@printer go "random_order"]
  | Reason [@name "reason"] [@printer go "reason"]
  | Recipient [@name "recipient"] [@printer go "recipient"]
  | Redirect [@name "redirect"] [@printer go "redirect"]
  | Redirected [@name "redirected"] [@printer go "redirected"]
  | RegistrationDisabled [@name "registration_disabled"]
  [@printer go "registration_disabled"]
  | RegistrationPossible [@name "registration_possible"]
  [@printer go "registration_possible"]
  | Reminder [@name "reminder"] [@printer go "reminder"]
  | ReminderCount [@name "reminder_count"] [@printer go "reminder_count"]
  | RemindersSent [@name "reminders_sent"] [@printer go "reminders_sent"]
  | Required [@name "required"] [@printer go "required"]
  | ResentAt [@name "resent_at"] [@printer go "resent_at"]
  | Resource [@name "resource"] [@printer go "resource"]
  | Role [@name "role"] [@printer go "role"]
  | Room [@name "room"] [@printer go "room"]
  | Root [@name "root"] [@printer go "root"]
  | Rule [@name "rule"] [@printer go "rule"]
  | RunAt [@name "run_at"] [@printer go "run_at"]
  | ScheduledTime [@name "scheduled_time"] [@printer go "scheduled_time"]
  | ScheduledTimeSpan [@name "scheduled_time_span"] [@printer go "scheduled_time_span"]
  | Score [@name "score"] [@printer go "score"]
  | Search [@name "search"] [@printer go "search"]
  | SearchOf of t [@name "search_of"] [@printer nested "search_of" show]
  | SecondReminder [@name "second_reminder"] [@printer go "second_reminder"]
  | Sender [@name "sender"] [@printer go "sender"]
  | SendingInvitations [@name "sending_invitations"] [@printer go "sending_invitations"]
  | SentAt [@name "sent_at"] [@printer go "sent_at"]
  | Session [@name "session"] [@printer go "session"]
  | SessionCount [@name "session_count"] [@printer go "session_count"]
  | Sessions [@name "sessions"] [@printer go "sessions"]
  | Setting [@name "setting"] [@printer go "setting"]
  | Settings [@name "settings"] [@printer go "settings"]
  | ShowExteralDataIdLinks [@name "show_external_data_id_links"]
  [@printer go "show_external_data_id_links"]
  | ShowUpCount [@name "show_up_count"] [@printer go "show_up_count"]
  | ShowToAdmins [@name "show_to_admins"] [@printer go "show_to_admins"]
  | ShowToContacts [@name "show_to_contacts"] [@printer go "show_to_contacts"]
  | SignedUpAt [@name "signed_up_at"] [@printer go "signed_up_at"]
  | SignUpCode [@name "signup_code"] [@printer go "signup_code"]
  | SignUpCount [@name "sign_up_count"] [@printer go "sign_up_count"]
  | SMS [@name "sms"] [@printer go "sms"]
  | SmsText [@name "sms_text"] [@printer go "sms_text"]
  | Smtp [@name "smtp"] [@printer go "smtp"]
  | SmtpLabel [@name "smtp_label"] [@printer go "smtp_label"]
  | SmtpMechanism [@name "smtp_mechanism"] [@printer go "smtp_mechanism"]
  | SmtpPassword [@name "smtp_password"] [@printer go "smtp_password"]
  | SmtpPort [@name "smtp_port"] [@printer go "smtp_port"]
  | SmtpProtocol [@name "smtp_protocol"] [@printer go "smtp_protocol"]
  | SmtpServer [@name "smtp_server"] [@printer go "smtp_server"]
  | SmtpUsername [@name "smtp_username"] [@printer go "smtp_username"]
  | SortOrder [@name "sort_order"] [@printer go "sort_order"]
  | Start [@name "start"] [@printer go "start"]
  | StartNow [@name "start_now"] [@printer go "start_now"]
  | Status [@name "status"] [@printer go "status"]
  | Street [@name "street"] [@printer go "street"]
  | Styles [@name "styles"] [@printer go "styles"]
  | Successful [@name "successful"] [@printer go "successful"]
  | Survey [@name "survey"] [@printer go "survey"]
  | SurveyUrl [@name "survey_url"] [@printer go "survey_url"]
  | SystemEvent [@name "system_event"] [@printer go "system_event"]
  | Tag [@name "tag"] [@printer go "tag"]
  | Tagging [@name "tagging"] [@printer go "tagging"]
  | Tags [@name "tags"] [@printer go "tags"]
  | Target [@name "target"] [@printer go "target"]
  | Template [@name "template"] [@printer go "template"]
  | Tenant [@name "tenant"] [@printer go "tenant"]
  | TenantDisabledFlag [@name "tenant_disabled_flag"] [@printer go "tenant_disabled_flag"]
  | TenantId [@name "tenant_id"] [@printer go "tenant_id"]
  | TenantLogos [@name "tenant_logos"] [@printer go "tenant_logos"]
  | TenantMaintenanceFlag [@name "tenant_maintenance_flag"]
  [@printer go "tenant_maintenance_flag"]
  | TenantPool [@name "tenant_pool"] [@printer go "tenant_pool"]
  | TermsAccepted [@name "terms_accepted"] [@printer go "terms_accepted"]
  | TermsAcceptedCount [@name "terms_accepted_count"] [@printer go "terms_accepted_count"]
  | TermsAndConditions [@name "terms_and_conditions"] [@printer go "terms_and_conditions"]
  | TermsAndConditionsLastAccepted [@name "terms_and_conditions_last_accepted"]
  [@printer go "terms_and_conditions_last_accepted"]
  | TestPhoneNumber [@name "test_phone_number"] [@printer go "test_phone_number"]
  | Text [@name "text"] [@printer go "text"]
  | TextMessage [@name "text_message"] [@printer go "text_message"]
  | TextMessageDlrStatus [@name "text_message_dlr_status"]
  [@printer go "text_message_dlr_mask"]
  | TextMessageLeadTime [@name "text_message_lead_time"]
  [@printer go "text_message_lead_time"]
  | TextMessageRemindersSentAt [@name "text_message_reminders_sent_at"]
  [@printer go "text_message_reminders_sent_at"]
  | Time [@name "time"] [@printer go "time"]
  | TimeSpan [@name "timespan"] [@printer go "timespan"]
  | TimeUnit [@name "timeunit"] [@printer go "timeunit"]
  | TimeUnitOf of t [@name "timeunit_of"] [@printer nested "timeunit_of" show]
  | TimeWindow [@name "time_window"] [@printer go "time_window"]
  | Title [@name "title"] [@printer go "title"]
  | ToHandle [@name "to_handle"] [@printer go "to_handle"]
  | Token [@name "token"] [@printer go "token"]
  | Total [@name "total"] [@printer go "total"]
  | Translation [@name "translation"] [@printer go "translation"]
  | Tries [@name "tries"] [@printer go "tries"]
  | TriggerProfileUpdateAfter [@name "trigger_profile_update_after"]
  [@printer go "trigger_profile_update_after"]
  | UpdatedAt [@name "updated_at"] [@printer go "updated_at"]
  | Url [@name "url"] [@printer go "url"]
  | User [@name "user"] [@printer go "user"]
  | Validation [@name "validation"] [@printer go "validation"]
  | Value [@name "value"] [@printer go "value"]
  | ValueOf of t [@name "value_of"] [@printer nested "value_of" show]
  | VerificationCode [@name "verification_code"] [@printer go "verification_code"]
  | VerificationCount [@name "verification_count"] [@printer go "verification_count"]
  | Verified [@name "verified"] [@printer go "verified"]
  | Version [@name "version"] [@printer go "version"]
  | Virtual [@name "virtual"] [@printer go "virtual"]
  | WaitingList [@name "waiting_list"] [@printer go "waiting_list"]
  | Year [@name "year"] [@printer go "year"]
  | Zip [@name "zip"] [@printer go "zip"]
[@@deriving eq, show { with_path = false }, yojson, variants, sexp_of]

let read_nested str =
  let open CCString in
  match contains str '[' && contains str ']' with
  | false -> None
  | true ->
    let open CCList in
    (try
       str
       |> split_on_char '['
       |> function
       | [ "timeunit_of"; tl ] ->
         tl
         |> split_on_char ']'
         |> hd
         |> Utils.Json.read_variant t_of_yojson
         |> timeunitof
         |> CCOption.return
       | _ -> None
     with
     | _ -> None)
;;

let read str =
  read_nested str
  |> function
  | Some m -> m
  | None -> Utils.Json.read_variant t_of_yojson str
;;

let url_key m = m |> show |> Format.asprintf ":%s"
let array_key m = m |> show |> Format.asprintf "%s[]"
let human_url m = m |> show |> CCString.replace ~sub:"_" ~by:"-"
