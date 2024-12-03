open Tyxml.Html
open Statistics
module Field = Pool_message.Field

type title =
  | NavLink of Pool_common.I18n.nav_link
  | Text of Pool_common.I18n.t

let create
      language
      ( period
      , { active_contacts
        ; pending_contact_imports
        ; login_count
        ; sign_up_count
        ; terms_accepted_count
        ; terms_last_changed
        ; assignments_created
        ; invitations_sent
        ; reminders_sent
        ; emails_sent
        } )
  =
  let open Pool_common in
  let open Statistics in
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
  let create_table ?title figures =
    let title =
      title
      |> CCOption.map_or ~default:(txt "") (fun title ->
        let text =
          match title with
          | NavLink navlink -> Utils.nav_link_to_string language navlink
          | Text text -> Utils.text_to_string language text
        in
        h4 ~a:[ a_class [ "heading-4" ] ] [ txt text ])
    in
    figures
    |> CCList.map (fun (field, value, hint) ->
      let field =
        field
        |> Pool_common.Utils.field_to_string language
        |> CCString.capitalize_ascii
        |> txt
      in
      let head =
        match hint with
        | None -> [ field ]
        | Some hint -> [ field; br (); i [ txt hint ] ]
      in
      tr [ th ~a:[ a_class [ "w-10" ] ] head; td [ value ] ])
    |> table ~a:[ a_class (Component_table.table_classes `Simple ~align_top:true ()) ]
    |> fun figures -> div [ title; figures ]
  in
  let contact_counters =
    [ ActiveContacts.(field, to_txt (value active_contacts), None)
    ; PendingContactImports.(field, to_txt (value pending_contact_imports), None)
    ]
  in
  let user_figures =
    [ LoginCount.(field, to_txt (value login_count), None)
    ; SignUpCount.(field, to_txt (value sign_up_count), None)
    ; TermsAcceptedCount.(
        ( field
        , to_txt (value terms_accepted_count)
        , Some
            Pool_common.(
              Utils.text_to_string
                language
                (I18n.TermsAndConditionsLastUpdated terms_last_changed)) ))
    ]
  in
  let experiment_figures =
    [ AssignmentsCreated.(field, to_txt (value assignments_created), None)
    ; InvitationsSent.(field, to_txt (value invitations_sent), None)
    ; RemindersSent.(field, to_txt (value reminders_sent), None)
    ]
  in
  let system_figures = [ EmailsSent.(field, to_txt (value emails_sent), None) ] in
  div
    ~a:[ a_class [ "flexcolumn"; "stack" ]; a_user_data "statistics" "" ]
    Pool_common.I18n.
      [ create_table contact_counters
      ; div
          ~a:[ a_class [ "border"; "inset"; "bg-grey-light" ] ]
          [ h3
              ~a:[ a_class [ "heading-3" ] ]
              Pool_common.
                [ txt
                    (Utils.text_to_string language I18n.Activity
                     |> CCString.capitalize_ascii)
                ]
          ; div
              ~a:[ a_class [ "stack" ] ]
              [ period_select
              ; create_table ~title:(NavLink Contacts) user_figures
              ; create_table ~title:(NavLink Experiments) experiment_figures
              ; create_table ~title:(Text System) system_figures
              ]
          ]
      ]
;;

module ExperimentFilter = struct
  open ExperimentFilter

  let create
        language
        { contacts_meeting_criteria; invitation_count; assigned_contacts_not_matching }
    =
    let open Pool_common in
    let to_string = Utils.text_to_string language in
    table
      ~a:[ a_class [ "table"; "simple"; "width-auto" ] ]
      [ tr
          [ th [ txt (to_string I18n.FilterNrOfContacts) ]
          ; td [ span [ txt (CCInt.to_string contacts_meeting_criteria) ] ]
          ]
      ; tr
          [ th [ txt (to_string I18n.FilterNrOfSentInvitations) ]
          ; td [ txt (CCInt.to_string invitation_count) ]
          ]
      ; tr
          [ th [ txt (to_string I18n.FilterNrOfUnsuitableAssignments) ]
          ; td [ txt (CCInt.to_string assigned_contacts_not_matching) ]
          ]
      ]
  ;;
end
