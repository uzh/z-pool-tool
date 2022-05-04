module Experiment = Page_subject_experiment
open Tyxml.Html

let dashboard Pool_context.{ language; _ } =
  div
    [ h1 [ txt Pool_common.(Utils.text_to_string language I18n.DashboardTitle) ]
    ]
;;

let sign_up = Page_subject_signup.signup
let terms = Page_subject_terms.terms
let detail = Page_subject_edit.detail
let edit = Page_subject_edit.edit
