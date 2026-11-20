open CCFun
open Tyxml.Html
open Pool_message
module Input = Component.Input

let path = Http_utils.Url.Admin.Settings.signup_codes_path
let webhook_example_url = "https://example.com/webhook"
let code_placeholder = "..."
let webhooks_config_id = "webhooks-config"
let payload_tag_classnames = [ "tag"; "success"; "ghost" ]
let payload_param key = Format.asprintf "&%s=<...>" key

let signup_url tenant_url code =
  add_field_query_params
    (Pool_tenant.create_public_url tenant_url "/signup")
    Signup_code.[ url_key, Code.value code ]
;;

let signup_url_preview tenant_url code =
  [ txt (signup_url tenant_url code)
  ; span
      ~a:[ a_class payload_tag_classnames ]
      [ txt (payload_param (Field.human_url Signup_code.payload_url_key)) ]
  ]
;;

let copy_button ?(icon_only = false) language value =
  let label = Pool_common.(Utils.control_to_string language Control.Copy) in
  let classnames, content =
    if icon_only
    then [ "is-icon" ], []
    else [ "small"; "has-icon" ], [ span [ txt label ] ]
  in
  button
    ~a:
      [ a_button_type `Button
      ; a_class ("primary" :: classnames)
      ; a_title label
      ; a_user_data "clipboard" value
      ]
    (Component.Icon.(to_html ClipboardOutline) :: content)
;;

let list Pool_context.{ language; _ } tenant_url (codes, query) =
  let open Signup_code in
  let url = Uri.of_string (path ()) in
  let data_table =
    Component.DataTable.create_meta ~search:searchable_by url query language
  in
  let cols =
    let create_button : [ | Html_types.flow5 ] elt =
      Input.link_as_button
        ~style:`Success
        ~icon:Component.Icon.Add
        ~control:(language, Control.Add (Some Field.SignUpCode))
        (path ~suffix:"create" ())
    in
    [ `column column_code
    ; `column column_signup_count
    ; `column column_verification_count
    ; `custom create_button
    ]
  in
  let row { id; code; signup_count; verification_count; _ } =
    [ txt (Code.value code)
    ; txt (Count.value signup_count |> CCInt.to_string)
    ; txt (Count.value verification_count |> CCInt.to_string)
    ; div
        ~a:[ a_class [ "flexrow"; "flex-gap-xs"; "justify-end" ] ]
        [ copy_button ~icon_only:true language (signup_url tenant_url code)
        ; Input.edit_link (path ~id ())
        ]
    ]
    |> CCList.map (CCList.return %> td)
    |> tr
  in
  Component.DataTable.make ~target_id:"signup-codes-list" ~cols ~row data_table codes
;;

let webhook_preview_script =
  Format.asprintf
    {js|
      (() => {
        const code = document.getElementById("%s");
        const inputs = document.querySelectorAll("[data-webhook-preview]");
        const payloadTag = () => {
          const tag = document.createElement("span");
          tag.className = "%s";
          tag.textContent = "%s";
          return tag;
        };
        const splitAtPayload = (value) => {
          const url = new URL(value);
          if (!["http:", "https:"].includes(url.protocol)) {
            throw new TypeError("Unsupported protocol");
          }
          const query = url.search ? `&${url.search.slice(1)}` : "";
          const hash = url.hash;
          const codeValue = encodeURIComponent(code.value.trim() || "%s");
          url.search = `%s=${codeValue}`;
          url.hash = "";
          return [url.toString(), `${query}${hash}`];
        };
        const update = (input) => {
          const preview = document.getElementById(input.dataset.webhookPreview);
          try {
            const [beforePayload, afterPayload] = splitAtPayload(
              input.value.trim() || input.placeholder
            );
            preview.replaceChildren(`GET ${beforePayload}`, payloadTag(), afterPayload);
          } catch {
            preview.replaceChildren();
          }
        };
        const updateAll = () => inputs.forEach(update);
        inputs.forEach((input) => input.addEventListener("input", () => update(input)));
        code.addEventListener("input", updateAll);
        updateAll();
      })();
    |js}
    (Field.show Field.Code)
    (CCString.concat " " payload_tag_classnames)
    (payload_param Signup_code.Webhook.payload_query_key)
    code_placeholder
    Signup_code.Webhook.code_query_key
;;

let webhooks_toggle_script =
  Format.asprintf
    {js|
      (() => {
        const toggle = document.getElementById("%s");
        const config = document.getElementById("%s");
        toggle.addEventListener("change", () => {
          config.classList.toggle("hidden", !toggle.checked);
          config.querySelectorAll("input").forEach((input) => {
            input.disabled = !toggle.checked;
          });
        });
      })();
    |js}
    (Field.show Field.WebhooksEnabled)
    webhooks_config_id
;;

let form Pool_context.{ language; csrf; flash_fetcher; _ } ~code_section signup_code =
  let open Signup_code in
  let action, control =
    match signup_code with
    | None -> path (), Control.Create (Some Field.SignUpCode)
    | Some { id; _ } -> path ~id (), Control.Update (Some Field.SignUpCode)
  in
  let webhooks_enabled =
    let has_webhook { signed_up_webhook_url; email_verified_webhook_url; _ } =
      CCOption.(is_some signed_up_webhook_url || is_some email_verified_webhook_url)
    in
    CCOption.bind flash_fetcher (fun fetch -> fetch (Field.show Field.WebhooksEnabled))
    |> CCOption.map_or
         ~default:(CCOption.map_or ~default:false has_webhook signup_code)
         (CCString.equal "true")
  in
  let webhook_input field hint webhook_url =
    let preview_id = Format.asprintf "%s-preview" (Field.show field) in
    Input.input_element
      ?flash_fetcher
      ~label_classnames:[ "font-bold" ]
      ~disabled:(not webhooks_enabled)
      ?value:(CCOption.bind signup_code webhook_url |> CCOption.map WebhookUrl.value)
      ~hints:[ hint ]
      ~additional_attributes:
        [ a_placeholder webhook_example_url; a_user_data "webhook-preview" preview_id ]
      ~append_html:
        [ div
            ~a:[ a_class [ "help" ]; a_style "margin-top: 0" ]
            [ code ~a:[ a_id preview_id; a_class [ "word-wrap-break-all" ] ] [] ]
        ]
      language
      `Url
      field
  in
  let webhooks =
    let open Pool_common in
    div
      ~a:[ a_class [ "stack" ] ]
      [ div
          ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
          [ div
              ~a:[ a_class [ "stack-xs"; "grow" ] ]
              [ h2
                  ~a:[ a_class [ "heading-2" ] ]
                  [ label
                      ~a:[ a_label_for (Field.show Field.WebhooksEnabled) ]
                      [ txt (Utils.text_to_string language I18n.Webhooks) ]
                  ]
              ; p [ txt (Utils.hint_to_string language I18n.SignUpCodeWebhooksHint) ]
              ]
          ; div
              [ Input.checkbox_element
                  ~classnames:[ "" ]
                  ~as_switch:true
                  ~hide_label:true
                  ~value:webhooks_enabled
                  language
                  Field.WebhooksEnabled
              ]
          ]
      ; div
          ~a:
            [ a_id webhooks_config_id
            ; a_class ("stack-lg" :: (if webhooks_enabled then [] else [ "hidden" ]))
            ]
          [ webhook_input
              Field.SignedUpWebhookUrl
              I18n.SignUpCodeSignedUpWebhookHint
              (fun { signed_up_webhook_url; _ } -> signed_up_webhook_url)
          ; webhook_input
              Field.EmailVerifiedWebhookUrl
              I18n.SignUpCodeEmailVerifiedWebhookHint
              (fun { email_verified_webhook_url; _ } -> email_verified_webhook_url)
          ]
      ]
  in
  form
    ~a:
      [ a_action (Sihl.Web.externalize_path action)
      ; a_method `Post
      ; a_class [ "stack-lg" ]
      ]
    [ Input.csrf_element csrf ()
    ; code_section
    ; webhooks
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ Input.submit_element ~classnames:[ "push" ] language control () ]
    ; script (Unsafe.data webhook_preview_script)
    ; script (Unsafe.data webhooks_toggle_script)
    ]
;;

let layout title children =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    (h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ txt title ] :: children)
;;

let index (Pool_context.{ language; _ } as context) tenant_url codes =
  layout
    Pool_common.(Utils.nav_link_to_string language I18n.SignupCodes)
    [ p [ txt Pool_common.(Utils.hint_to_string language I18n.SignUpCodeHint) ]
    ; div ~a:[ a_class [ "gap-lg" ] ] [ list context tenant_url codes ]
    ]
;;

let new_form (Pool_context.{ language; flash_fetcher; _ } as context) =
  let code_section =
    Input.input_element ?flash_fetcher ~required:true language `Text Field.Code
  in
  layout
    Pool_common.(
      Utils.control_to_string language (Control.Create (Some Field.SignUpCode)))
    [ form context ~code_section None ]
;;

let edit (Pool_context.{ language; _ } as context) tenant_url signup_code =
  let open Signup_code in
  let url = signup_url tenant_url signup_code.code in
  let code_section =
    let open Pool_common in
    div
      ~a:[ a_class [ "stack" ] ]
      [ Input.input_element
          ~disabled:true
          ~value:(Code.value signup_code.code)
          language
          `Text
          Field.Code
      ; div
          ~a:[ a_class [ "card" ]; a_style "margin-bottom: 2rem" ]
          [ div
              ~a:[ a_class [ "card-header" ] ]
              [ strong
                  [ txt (Utils.field_to_string_capitalized language Field.SignUpUrl) ]
              ]
          ; div
              ~a:[ a_class [ "card-body" ] ]
              [ div
                  ~a:[ a_class [ "flexrow"; "flex-gap"; "align-center" ] ]
                  [ code
                      ~a:[ a_class [ "grow" ] ]
                      (signup_url_preview tenant_url signup_code.code)
                  ; copy_button language url
                  ]
              ; p
                  ~a:[ a_class [ "help" ] ]
                  [ txt (Utils.hint_to_string language I18n.SignUpCodeUrlHint) ]
              ]
          ]
      ]
  in
  layout
    Pool_common.(Utils.control_to_string language (Control.Edit (Some Field.SignUpCode)))
    [ form context ~code_section (Some signup_code) ]
;;
