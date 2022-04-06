open Entity

let get_or_failwith = Pool_common.Utils.get_or_failwith
let languages = Pool_common.Language.[ En; De ]

let email_suffix =
  CCList.map
    (fun m -> m |> EmailSuffix.create |> get_or_failwith)
    [ "econ.uzh.ch"; "uzh.ch" ]
;;

let contact_email = ContactEmail.create "pool@econ.uzh.ch" |> get_or_failwith

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
