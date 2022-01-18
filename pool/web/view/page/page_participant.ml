open Tyxml.Html

let dashboard language message () =
  let html =
    div
      [ h1
          [ txt Pool_common.(Utils.text_to_string language I18n.DashboardTitle)
          ]
      ]
  in
  Page_layout.create html message language ()
;;

let sign_up = Page_participant_signup.signup
let terms = Page_participant_terms.terms
let detail = Page_participant_edit.detail
let edit = Page_participant_edit.edit
