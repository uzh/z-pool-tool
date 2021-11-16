let signup
    csrf
    message
    channels
    email
    firstname
    lastname
    recruitment_channel
    terms
  =
  let submit_url = Sihl.Web.externalize_path "/participant/signup" in
  let email = email |> CCOption.value ~default:"" in
  let firstname = firstname |> CCOption.value ~default:"" in
  let lastname = lastname |> CCOption.value ~default:"" in
  let children =
    let open Tyxml.Html in
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
      [ h1 [ txt "Participant SignUp" ]
      ; form
          ~a:[ a_action submit_url; a_method `Post ]
          [ Component.csrf_element csrf ()
          ; div
              [ label [ txt "Email" ]
              ; input
                  ~a:
                    [ a_placeholder "example@mail.com"
                    ; a_required ()
                    ; a_name "email"
                    ; a_value email
                    ; a_input_type `Email
                    ]
                  ()
              ]
          ; div
              [ label [ txt "Firstname" ]
              ; input
                  ~a:
                    [ a_placeholder "Firstname"
                    ; a_required ()
                    ; a_name "firstname"
                    ; a_value firstname
                    ; a_input_type `Text
                    ]
                  ()
              ]
          ; div
              [ label [ txt "Lastname" ]
              ; input
                  ~a:
                    [ a_placeholder "Lastname"
                    ; a_required ()
                    ; a_name "lastname"
                    ; a_value lastname
                    ; a_input_type `Text
                    ]
                  ()
              ]
          ; div
              [ label [ txt "Password" ]
              ; input
                  ~a:
                    [ a_placeholder "Password"
                    ; a_required ()
                    ; a_name "password"
                    ; a_input_type `Password
                    ]
                  ()
              ]
          ; div
              [ label [ txt "Recruitment Channel" ]
              ; select
                  ~a:[ a_required (); a_name "recruitment_channel" ]
                  channel_select
              ]
          ; div
              [ p [ txt (terms |> Settings.TermsAndConditions.value) ]
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
                      [ txt "I accept the terms and conditions." ]
                  ]
              ]
          ; button ~a:[ a_button_type `Submit ] [ txt "Sign Up" ]
          ]
      ]
  in
  Page_layout.create children message
;;
