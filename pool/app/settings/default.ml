open Entity

type default =
  { default_reminder_lead_time : Value.default_reminder_lead_time
  ; tenant_languages : Value.tenant_languages
  ; tenant_email_suffixes : Value.tenant_email_suffixes
  ; tenant_contact_email : Value.tenant_contact_email
  ; inactive_user_disable_after : Value.inactive_user_disable_after
  ; inactive_user_warning : Value.inactive_user_warning
  ; terms_and_conditions : Value.terms_and_conditions
  }
[@@deriving eq, show]

let get_or_failwith = Pool_common.Utils.get_or_failwith
let tenant_languages = Pool_common.Language.[ En; De ]

let default_reminder_lead_time =
  14400
  |> Ptime.Span.of_int_s
  |> Pool_common.Reminder.LeadTime.create
  |> get_or_failwith
;;

let tenant_email_suffixes =
  CCList.map
    (fun m -> m |> EmailSuffix.create |> get_or_failwith)
    [ "econ.uzh.ch"; "uzh.ch" ]
;;

let tenant_contact_email =
  ContactEmail.create "pool@econ.uzh.ch" |> get_or_failwith
;;

let inactive_user_disable_after =
  InactiveUser.DisableAfter.create "5" |> get_or_failwith
;;

let inactive_user_warning = InactiveUser.Warning.create "7" |> get_or_failwith

let terms_and_conditions =
  [ "EN", "Please update the terms and conditions in the tenant settings!"
  ; ( "DE"
    , "Die Nutzungsbedingungen können in den Tenant Einstellungen angepasst \
       werden." )
  ]
  |> CCList.map (fun (language, text) ->
         TermsAndConditions.create language text |> get_or_failwith)
;;

let default_values =
  { default_reminder_lead_time
  ; tenant_languages
  ; tenant_email_suffixes
  ; tenant_contact_email
  ; inactive_user_disable_after
  ; inactive_user_warning
  ; terms_and_conditions
  }
;;
