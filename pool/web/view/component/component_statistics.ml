open Tyxml.Html
open Statistics
module Field = Pool_common.Message.Field

let create
  language
  ( period
  , { active_contacts
    ; pending_contact_imports
    ; assignments_created
    ; invitations_sent
    ; login_count
    ; sign_up_count
    } )
  =
  let to_txt value = txt (CCInt.to_string value) in
  let period_select =
    let attributes =
      [ a_user_data "hx-get" (Sihl.Web.externalize_path "/admin/statistics")
      ; a_user_data "hx-swap" "outerHTML"
      ; a_user_data "hx-target" "closest [data-statistics]"
      ]
    in
    Component_input.selector
      ~option_formatter:(period_to_human language)
      ~attributes
      language
      Field.Period
      show_period
      all_periods
      period
      ()
  in
  Field.
    [ ActiveContactsCount, to_txt (ActiveContacts.value active_contacts)
    ; ( PendingContactImports
      , to_txt (PendingContactImports.value pending_contact_imports) )
    ; AssignmentsCreated, to_txt (AssignmentsCreated.value assignments_created)
    ; InvitationsSent, to_txt (InvitationsSent.value invitations_sent)
    ; LoginCount, to_txt (LoginCount.value login_count)
    ; SignUpCount, to_txt (SignUpCount.value sign_up_count)
    ]
  |> Component_table.vertical_table `Striped language
  |> fun table ->
  div
    ~a:[ a_class [ "flexcolumn"; "stack" ]; a_user_data "statistics" "" ]
    [ period_select; table ]
;;
