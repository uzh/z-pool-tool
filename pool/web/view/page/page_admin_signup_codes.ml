open Tyxml.Html

let list Pool_context.{ language; _ } (admins, query) =
  let open Signup_code in
  let url = Uri.of_string Http_utils.Url.Admin.Settings.signup_codes_path in
  let data_table =
    Component.DataTable.create_meta ~search:searchable_by url query language
  in
  let cols =
    [ `column column_code
    ; `column column_signup_count
    ; `column column_verification_count
    ]
  in
  let row { code; signup_count; verification_count; _ } =
    [ Code.value code
    ; Count.value signup_count |> CCInt.to_string
    ; Count.value verification_count |> CCInt.to_string
    ]
    |> CCList.map (fun x -> td [ txt x ])
    |> tr
  in
  Component.DataTable.make
    ~target_id:"signup-codes-list"
    ~cols
    ~row
    data_table
    admins
;;

let generate_code langauge tenant_url =
  let open Pool_message in
  let input_id = "generate" in
  let output_id = "generated_url" in
  let button_id = "generate_button" in
  let public_url = Pool_tenant.Url.value tenant_url in
  let name = Pool_common.Utils.field_to_string langauge Field.Code in
  let key = Field.human_url Signup_code.url_key in
  let generate_js =
    Format.asprintf
      {js|
        const buttonId = "%s";
        const urlKey = "%s";
        const publicUrl = "%s";
        const targetElId = "%s";
        const inputId = "%s"; 

        const targetEl = document.getElementById(targetElId);
        const inputEl = document.getElementById(inputId);

        const btn = document.getElementById(buttonId);
        btn.addEventListener("click", (e) => {
          console.log("CLICK");
          e.preventDefault();

          if(inputEl.value) {
            targetEl.innerHTML = `${publicUrl}/signup?${urlKey}=${inputEl.value}`;
          }
        })
      |js}
      button_id
      key
      public_url
      output_id
      input_id
  in
  div
    [ div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ div
            ~a:[ a_class [ "form-group" ] ]
            [ div
                ~a:[ a_class [ "flexrow"; "flex-gap" ] ]
                [ input
                    ~a:
                      [ a_input_type `Text
                      ; a_id input_id
                      ; a_placeholder (CCString.capitalize_ascii name)
                      ; a_name name
                      ; a_class [ "grow" ]
                      ]
                    ()
                ; Component.Input.submit_element
                    ~attributes:[ a_id "generate_button" ]
                    langauge
                    Control.Generate
                    ()
                ]
            ]
        ; div
            ~a:[ a_class [ "flexrow"; "align-center" ] ]
            [ span ~a:[ a_id output_id ] [] ]
        ]
    ; script (Unsafe.data generate_js)
    ]
;;

let index tenant_url (Pool_context.{ language; _ } as context) codes =
  let open Pool_common in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt (Utils.nav_link_to_string language I18n.SignupCodes) ]
    ; p [ txt (Utils.hint_to_string language I18n.SignUpCodeHint) ]
    ; generate_code language tenant_url
    ; div ~a:[ a_class [ "gap-lg" ] ] [ list context codes ]
    ]
;;
