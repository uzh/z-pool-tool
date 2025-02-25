open Tyxml.Html
open Statistics
open Experiment
module Field = Pool_message.Field

type title =
  | NavLink of Pool_common.I18n.nav_link
  | Text of Pool_common.I18n.t

module Pool = struct
  open Pool

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
end

module SentInvitations = struct
  open Statistics.SentInvitations

  let create language { invitation_resets; sent_since_last_reset; total_match_filter } =
    let text_to_string = Pool_common.Utils.text_to_string language in
    let field_to_string field =
      Pool_common.Utils.field_to_string language field |> CCString.capitalize_ascii
    in
    let thead =
      thead
        Field.
          [ tr
              [ td [ text_to_string Pool_common.I18n.Reset |> txt ]
              ; td [ field_to_string InvitationCount |> txt ]
              ; td [ field_to_string MatchingFilterCountShort |> txt ]
              ]
          ]
    in
    let make_row reset_at (num_invitations, num_matching_filter) =
      tr
        [ td [ reset_at |> txt ]
        ; td [ num_invitations |> CCInt.to_string |> txt ]
        ; td [ num_matching_filter |> CCInt.to_string |> txt ]
        ]
    in
    match invitation_resets, sent_since_last_reset with
    | [], 0 -> p [ strong [ txt (text_to_string Pool_common.I18n.NoInvitationsSent) ] ]
    | resets, _ ->
      let resets_rows =
        CCList.map
          (fun InvitationReset.
                 { created_at; contacts_matching_filter; invitations_sent; _ } ->
             make_row
               (created_at
                |> Pool_common.CreatedAt.value
                |> Utils.Ptime.formatted_date_time)
               (invitations_sent, contacts_matching_filter))
          resets
      in
      let current_row = make_row "Current" (sent_since_last_reset, total_match_filter) in
      resets_rows @ [ current_row ] |> table ~thead ~a:[ a_class [ "table"; "simple" ] ]
  ;;
end

module ExperimentFilter = struct
  open ExperimentFilter

  let create
        language
        { total_sent
        ; total_match_filter
        ; total_uninvited_matching
        ; assigned_contacts_not_matching
        ; sent_invitations
        }
    =
    let to_string = CCInt.to_string in
    let text_to_string = Pool_common.Utils.text_to_string language in
    let table =
      let sent_invitations =
        let open InvitationReset in
        let open Statistics.SentInvitations in
        let row key value =
          tr
            ~a:[ a_class [ "font-italic" ] ]
            [ td [ span ~a:[ a_class [ "inset"; "left" ] ] [ txt key ] ]
            ; td [ txt (CCInt.to_string value) ]
            ]
        in
        let resets =
          sent_invitations.invitation_resets
          |> CCList.map (fun { iteration; invitations_sent; _ } ->
            row (CCInt.to_string iteration) invitations_sent)
        in
        resets @ [ row "Current" sent_invitations.sent_since_last_reset ]
      in
      let table_row (label, value) =
        tr [ td [ txt (text_to_string label) ]; td [ txt (to_string value) ] ]
      in
      let open Pool_common in
      [ table_row (I18n.FilterNrOfContacts, total_match_filter)
      ; table_row (I18n.FilterNrOfSentInvitations, total_sent)
      ]
      @ sent_invitations
      @ [ table_row (I18n.FilterNuberMatchingUninvited, total_uninvited_matching)
        ; table_row (I18n.FilterNrOfUnsuitableAssignments, assigned_contacts_not_matching)
        ]
      |> table ~a:[ a_class [ "table"; "simple"; "width-auto" ] ]
    in
    table
  ;;
end

module ExperimentOverview = struct
  let make language statistics =
    let int_to_txt i = i |> CCInt.to_string |> txt in
    let with_tooltip html tooltip =
      div
        ~a:[ a_class [ "has-icon flex-gap-sm" ] ]
        [ span [ html ]
        ; div
            ~a:[ a_class [ "tooltip-wrapper" ] ]
            [ Component_icon.(to_html HelpOutline)
            ; p ~a:[ a_class [ "tooltip" ] ] [ tooltip ]
            ]
        ]
    in
    let to_table = Component_table.vertical_table ~th_class:[ "w-7" ] `Simple language in
    let registration_possible_html =
      let open Statistics in
      let open RegistrationPossible in
      let html =
        statistics
        |> registration_possible
        |> value
        |> Pool_common.Utils.bool_to_string language
        |> txt
      in
      let tooltip = hint |> Pool_common.Utils.hint_to_string language |> txt in
      with_tooltip html tooltip
    in
    let sending_invitations_html =
      let open Statistics in
      let open SendingInvitations in
      let html =
        statistics |> sending_invitations |> show |> CCString.capitalize_ascii |> txt
      in
      let tooltip =
        hint |> Pool_common.Utils.hint_to_string language |> Http_utils.add_line_breaks
      in
      with_tooltip html tooltip
    in
    let experiment_statistics =
      let open Statistics in
      [ RegistrationPossible.field, registration_possible_html
      ; SendingInvitations.field, sending_invitations_html
      ; SessionCount.(field, statistics |> session_count |> value |> int_to_txt)
      ]
    in
    let invitations_statistics =
      statistics.Statistics.invitations |> SentInvitations.create language
    in
    let assignments_statistics =
      let open Statistics in
      [ ShowUpCount.(field, statistics |> showup_count |> value |> int_to_txt)
      ; NoShowCount.(field, statistics |> noshow_count |> value |> int_to_txt)
      ; ParticipationCount.(
          field, statistics |> participation_count |> value |> int_to_txt)
      ]
    in
    div
      [ h3 [ txt Pool_common.(Utils.text_to_string language I18n.ExperimentStatistics) ]
      ; experiment_statistics |> to_table
      ; h4 [ txt Pool_common.(Utils.nav_link_to_string language I18n.Invitations) ]
      ; invitations_statistics
      ; h4 [ txt Pool_common.(Utils.nav_link_to_string language I18n.Assignments) ]
      ; assignments_statistics |> to_table
      ]
  ;;
end
