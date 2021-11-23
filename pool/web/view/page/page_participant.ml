open Tyxml.Html

let dashboard message () =
  let html = div [ h1 [ txt "Participant Dashboard" ] ] in
  Page_layout.create html message ()
;;

let sign_up = Page_participant_signup.signup
let terms = Page_participant_terms.terms
let detail = Page_participant_edit.detail
let edit = Page_participant_edit.edit
