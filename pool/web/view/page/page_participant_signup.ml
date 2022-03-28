module Field = Pool_common.Message

let signup
    csrf
    channels
    email
    firstname
    lastname
    recruitment_channel
    terms
    Pool_context.{ language; query_language; _ }
  =
  let open Tyxml.Html in
  let field_to_string = Pool_common.Utils.field_to_string language in
  let submit_url =
    Http_utils.externalize_path_with_lang query_language "/signup"
  in
  let txt_to_string m = [ txt (Pool_common.Utils.text_to_string language m) ] in
  let input_element = Component.input_element language in
  let email = email |> CCOption.value ~default:"" in
  let firstname = firstname |> CCOption.value ~default:"" in
  let lastname = lastname |> CCOption.value ~default:"" in
  let channel_select =
    let default =
      option
        ~a:
          (match recruitment_channel with
          | None -> [ a_disabled (); a_selected () ]
          | Some _ -> [ a_disabled () ])
        (txt
           Pool_common.(Utils.control_to_string language (Message.Choose None)))
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
        ~a:[ a_action submit_url; a_method `Post; a_class [ "stack" ] ]
        [ Component.csrf_element csrf ()
        ; input_element `Email (Some "email") Pool_common.Message.Email email
        ; input_element
            `Text
            (Some "firstname")
            Pool_common.Message.Firstname
            firstname
        ; input_element
            `Text
            (Some "lastname")
            Pool_common.Message.Lastname
            lastname
        ; input_element
            `Password
            (Some "password")
            Pool_common.Message.Password
            ""
        ; div
            ~a:[ a_class [ "flex-box"; "flex--column" ] ]
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
        ; Component.submit_element
            language
            Pool_common.Message.(SignUp)
            ~classnames:[ "button--primary" ]
            ()
        ]
    ]
;;
