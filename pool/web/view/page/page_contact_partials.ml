open Tyxml.Html

let toggle_status_form
      csrf
      language
      query_params
      ?has_icon
      ~action
      ?confirmable
      ~submit_type
      ~control
      ()
  =
  let open Component.Input in
  let externalize = Http_utils.externalize_path_with_params query_params in
  let confirmable =
    confirmable
    |> CCOption.map_or ~default:[] (fun confirmable ->
      [ a_user_data
          "confirmable"
          (Pool_common.Utils.confirmable_to_string language confirmable)
      ])
  in
  form
    ~a:([ a_method `Post; a_action (externalize action) ] @ confirmable)
    [ csrf_element csrf ()
    ; submit_element ?has_icon ~classnames:[ "nobr" ] ~submit_type language control ()
    ]
;;

let promote_form csrf language query_language contact =
  let open Pool_common in
  let action =
    Format.asprintf "/admin/contacts/%s/promote" Contact.(contact |> id |> Id.value)
  in
  let hint : I18n.hint = I18n.PromoteContact in
  let confirmable : I18n.confirmable = I18n.PromoteContact in
  let form =
    toggle_status_form
      csrf
      language
      query_language
      ~action
      ~confirmable
      ~submit_type:`Success
      ~control:Pool_message.Control.PromoteContact
      ()
  in
  form, hint
;;

let mark_as_deleted_form csrf language query_language contact =
  let open Pool_common in
  let action =
    Format.asprintf "/admin/contacts/%s/delete" Contact.(contact |> id |> Id.value)
  in
  let hint : I18n.hint = I18n.DeleteContact in
  let confirmable : I18n.confirmable = I18n.DeleteContact in
  let form =
    toggle_status_form
      csrf
      language
      query_language
      ~has_icon:Component.Icon.TrashOutline
      ~action
      ~confirmable
      ~submit_type:`Error
      ~control:Pool_message.(Control.Delete (Some Pool_message.Field.Contact))
      ()
  in
  form, hint
;;

let pause_form csrf language query_language contact form_context =
  let open Pool_common in
  let action, hint =
    match form_context with
    | `Contact -> Htmx.contact_profile_hx_post, I18n.PauseAccountContact
    | `Admin -> Htmx.admin_profile_hx_post (Contact.id contact), I18n.PauseAccountAdmin
  in
  let action = Format.asprintf "%s/pause" action in
  let control, confirmable, submit_type =
    let open Pool_message.Control in
    let open Pool_common in
    match contact.Contact.paused |> Pool_user.Paused.value with
    | true -> ReactivateAccount, I18n.ReactivateAccount, `Success
    | false -> PauseAccount, I18n.PauseAccount, `Error
  in
  let form =
    toggle_status_form
      csrf
      language
      query_language
      ~has_icon:Component.Icon.NotificationsOffOutline
      ~action
      ~confirmable
      ~submit_type
      ~control
      ()
  in
  form, hint
;;

let verify_form csrf language query_language contact =
  let open Pool_common in
  let action =
    Http_utils.Url.Admin.contact_path ~suffix:"verify" ~id:(Contact.id contact) ()
  in
  let control, submit_type =
    match contact.Contact.verified with
    | None -> Pool_message.Control.(Verify None), `Success
    | Some _ -> Pool_message.Control.Unverify, `Error
  in
  let form =
    toggle_status_form
      csrf
      language
      query_language
      ~has_icon:Component.Icon.CheckmarkCircleOutline
      ~action
      ~submit_type
      ~control
      ()
  in
  form, I18n.VerifyContact
;;

let status_form_table language forms =
  div
    [ h2
        ~a:[ a_class [ "heading-2" ] ]
        [ txt
            Pool_common.(
              Utils.field_to_string language Pool_message.Field.Status
              |> CCString.capitalize_ascii)
        ]
    ; forms
      |> CCList.map (fun (form, hint) ->
        tr [ td [ form ]; td [ txt Pool_common.(Utils.hint_to_string language hint) ] ])
      |> table ~a:[ a_class [ "table" ] ]
    ]
;;
