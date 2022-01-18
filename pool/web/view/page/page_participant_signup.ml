module Field = Pool_common.Message

let signup
    csrf
    message
    language
    query_language
    channels
    email
    firstname
    lastname
    recruitment_channel
    terms
  =
  let open Tyxml.Html in
  let field_to_string = Pool_common.Utils.field_to_string language in
  let submit_url =
    Http_utils.externalize_path_with_query_language "/signup" query_language
  in
  let txt_to_string m = [ txt (Pool_common.Utils.text_to_string language m) ] in
  let email = email |> CCOption.value ~default:"" in
  let firstname = firstname |> CCOption.value ~default:"" in
  let lastname = lastname |> CCOption.value ~default:"" in
  let children =
    let channel_select =
      let default =
        option
          ~a:
            (match recruitment_channel with
            | None -> [ a_disabled (); a_selected () ]
            | Some _ -> [ a_disabled () ])
          (txt "Choose")
      in
      channels
      |> CCList.map (fun channel ->
             let is_selected =
               recruitment_channel
               |> CCOption.map_or ~default:false (CCString.equal channel)
             in
             option
               ~a:
                 (if is_selected
                 then [ a_value channel; a_selected () ]
                 else [ a_value channel ])
               (txt channel))
      |> CCList.cons default
    in
    div
      [ h1 (txt_to_string Pool_common.I18n.SignUpTitle)
      ; form
          ~a:[ a_action submit_url; a_method `Post ]
          [ Component.csrf_element csrf ()
          ; div
              [ label [ txt (field_to_string Field.Email) ]
              ; input
                  ~a:
                    [ a_placeholder (field_to_string Field.Email)
                    ; a_required ()
                    ; a_name "email"
                    ; a_value email
                    ; a_input_type `Email
                    ]
                  ()
              ]
          ; div
              [ label [ txt (field_to_string Field.Firstname) ]
              ; input
                  ~a:
                    [ a_placeholder (field_to_string Field.Firstname)
                    ; a_required ()
                    ; a_name "firstname"
                    ; a_value firstname
                    ; a_input_type `Text
                    ]
                  ()
              ]
          ; div
              [ label [ txt (field_to_string Field.Lastname) ]
              ; input
                  ~a:
                    [ a_placeholder (field_to_string Field.Lastname)
                    ; a_required ()
                    ; a_name "lastname"
                    ; a_value lastname
                    ; a_input_type `Text
                    ]
                  ()
              ]
          ; div
              [ label [ txt (field_to_string Field.Password) ]
              ; input
                  ~a:
                    [ a_placeholder (field_to_string Field.Password)
                    ; a_required ()
                    ; a_name "password"
                    ; a_input_type `Password
                    ]
                  ()
              ]
          ; div
              [ label [ txt (field_to_string Field.RecruitmentChannel) ]
              ; select
                  ~a:[ a_required (); a_name "recruitment_channel" ]
                  channel_select
              ]
          ; div
              [ p [ txt (Settings.TermsAndConditions.Terms.value terms) ]
              ; div
                  [ input
                      ~a:
                        [ a_input_type `Checkbox
                        ; a_name "_terms_accepted"
                        ; a_required ()
                        ]
                      ()
                  ; label
                      ~a:[ a_label_for "_terms_accepted" ]
                      (txt_to_string
                         Pool_common.I18n.SignUpAcceptTermsAndConditions)
                  ]
              ]
          ; Component.submit_element language Pool_common.Message.(SignUp)
          ]
      ]
  in
  Page_layout.create children message
;;
