open Tyxml.Html
open CCFun.Infix
open Pool_message
module Input = Component.Input

let announcement_path = Http_utils.Url.Root.announcement_path
let field_to_string = Pool_common.Utils.field_to_string_capitalized

let list { Pool_context.language; _ } (annoucements, query) =
  let open Announcement in
  let url = announcement_path () |> Uri.of_string in
  let data_table = Component.DataTable.create_meta url query language in
  let create_btn =
    let open Component in
    announcement_path ~suffix:"new" ()
    |> Input.link_as_button
         ~style:`Success
         ~icon:Icon.Add
         ~control:(language, Control.Add None)
  in
  let custom field = `custom (txt (field_to_string language field)) in
  let cols =
    [ custom Field.Text
    ; `column column_start
    ; `column column_end
    ; custom Field.ShowToAdmins
    ; custom Field.ShowToContacts
    ; `custom create_btn
    ]
  in
  let row
    ({ id; start_at; end_at; text; show_to_admins; show_to_contacts; _ } : t)
    =
    let open CCOption in
    let edit_btn =
      let open Component in
      announcement_path ~id ~suffix:"edit" ()
      |> Input.link_as_button ~style:`Primary ~icon:Icon.Create
    in
    let format_time = Utils.Ptime.formatted_date_time in
    let bool_icon =
      let open Component.Icon in
      function
      | true -> to_html Checkmark
      | false -> to_html Close
    in
    [ Text.find language text |> Unsafe.data
    ; start_at |> map_or ~default:"" (StartAt.value %> format_time) |> txt
    ; end_at |> map_or ~default:"" (EndAt.value %> format_time) |> txt
    ; show_to_admins |> ShowToAdmins.value |> bool_icon
    ; show_to_contacts |> ShowToContacts.value |> bool_icon
    ; edit_btn
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make
    ~target_id:"announcements-list"
    ~cols
    ~row
    data_table
    annoucements
;;

let index (Pool_context.{ language; _ } as context) announcements =
  let open Pool_common in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.text_to_string language I18n.AnnouncementsListTitle) ]
    ; list context announcements
    ]
;;

let form
  { Pool_context.csrf; language; _ }
  ?announcement
  ?(flash_fetcher : (string -> string option) option)
  available_tenants
  system_languages
  =
  let open Announcement in
  let open CCOption.Infix in
  let announcement, tenants =
    match announcement with
    | Some (announcement, tenants) -> Some announcement, Some tenants
    | None -> None, None
  in
  let action =
    match announcement with
    | None -> announcement_path ()
    | Some announcement -> announcement_path ~id:announcement.id ()
  in
  let title =
    let field = Some Field.Announcement in
    match CCOption.is_some announcement with
    | false -> Control.Create field
    | true -> Control.Update field
  in
  let date_input field value =
    Input.date_time_picker_element
      ~min_value:(Ptime_clock.now ())
      ?flash_fetcher
      ?value
      language
      field
  in
  let text_inputs =
    system_languages
    |> CCList.map (fun lang ->
      let name =
        Format.asprintf
          "%s[%s]"
          Field.(show Text)
          (Pool_common.Language.show lang)
      in
      let value =
        let open CCOption in
        bind flash_fetcher (fun flash_fetcher -> flash_fetcher name)
        <+> (announcement >>= fun { text; _ } -> Text.find_opt lang text)
      in
      div
        ~a:[ a_class [ "form-group" ] ]
        [ label ~a:[ a_label_for name ] [ txt (Pool_common.Language.show lang) ]
        ; textarea
            ~a:[ a_class [ "rich-text" ]; a_name name; a_id name ]
            (txt (CCOption.value value ~default:""))
        ])
  in
  let tenant_select =
    let open Pool_tenant in
    let name = Field.(array_key Tenant) in
    let checkboxes =
      available_tenants
      |> CCList.map (fun { id; title; _ } ->
        let checked =
          let open CCOption in
          tenants
          >|= CCList.find_opt (fun tenant ->
            Pool_tenant.Id.equal id tenant.Pool_tenant.id)
          |> map_or ~default:false is_some
        in
        let id = Id.value id in
        div
          [ input
              ~a:
                ([ a_name name; a_id id; a_value id; a_input_type `Checkbox ]
                 @ if checked then [ a_checked () ] else [])
              ()
          ; label ~a:[ a_label_for id ] [ txt (Title.value title) ]
          ])
    in
    div
      ~a:[ a_class [ "form-group" ] ]
      [ p
          [ txt
              Pool_common.(
                Utils.text_to_string language I18n.AnnouncementsTenantSelect)
          ]
      ; div ~a:[ a_class [ "input-group" ] ] checkboxes
      ]
  in
  let show_to_html =
    div
      [ Input.checkbox_element
          ?flash_fetcher
          ?value:
            (announcement
             >|= fun { show_to_admins; _ } -> ShowToAdmins.value show_to_admins
            )
          language
          Field.ShowToAdmins
      ; Input.checkbox_element
          ?flash_fetcher
          ?value:
            (announcement
             >|= fun { show_to_contacts; _ } ->
             ShowToContacts.value show_to_contacts)
          language
          Field.ShowToContacts
      ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 [ txt (Pool_common.Utils.control_to_string language title) ]
    ; form
        ~a:
          [ a_method `Post
          ; a_action (Sihl.Web.externalize_path action)
          ; a_class [ "stack" ]
          ; a_user_data "detect-unsaved-changes" ""
          ]
        Input.
          [ csrf_element csrf ()
          ; div
              ~a:[ a_class [ "grid-col-2" ] ]
              [ date_input
                  Field.Start
                  (announcement
                   >>= fun { start_at; _ } -> start_at >|= StartAt.value)
              ; date_input
                  Field.End
                  (announcement >>= fun { end_at; _ } -> end_at >|= EndAt.value)
              ; div
                  ~a:[ a_class [ "full-width" ] ]
                  [ h2 [ txt (field_to_string language Field.Text) ]
                  ; div ~a:[ a_class [ "stack" ] ] text_inputs
                  ]
              ; tenant_select
              ; show_to_html
              ; div
                  ~a:[ a_class [ "full-width"; "flexrow"; "justify-end" ] ]
                  [ submit_element language title () ]
              ]
          ]
    ]
;;
