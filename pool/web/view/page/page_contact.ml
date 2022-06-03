module Experiment = Page_contact_experiment
open Tyxml.Html

let dashboard Pool_context.{ language; _ } =
  div
    ~a:[ a_class [ "trim"; "measure"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.text_to_string language I18n.DashboardTitle) ]
    ]
;;

let sign_up = Page_contact_signup.signup
let terms = Page_contact_terms.terms
let detail = Page_contact_edit.detail
let edit = Page_contact_edit.edit
