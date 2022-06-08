open Tyxml.Html

module Partials = struct
  open Assignment

  let empty language =
    Pool_common.(
      Utils.text_to_string language (I18n.EmtpyList Message.Field.Assignments))
    |> txt
  ;;

  let contact_fullname (a : Assignment.t) = a.contact |> Contact.fullname |> txt

  let contact_email (a : Assignment.t) =
    a.contact |> Contact.email_address |> Pool_user.EmailAddress.value |> txt
  ;;

  let overview_list language ?session assignments =
    match CCList.is_empty assignments with
    | true -> p [ language |> empty ]
    | false ->
      CCList.map
        (fun (assignment : Assignment.t) ->
          let cells =
            [ td [ assignment |> contact_fullname ]
            ; td [ assignment |> contact_email ]
            ]
          in
          (match session with
          | Some session ->
            cells @ [ td [ txt (session |> Session.session_date_to_human) ] ]
          | None -> cells)
          |> tr)
        assignments
      |> table ~a:[ a_class [ "striped" ] ]
  ;;
end

let list assignments experiment Pool_context.{ language; _ } =
  let html =
    CCList.map
      (fun (session, assignments) ->
        div
          [ h2
              ~a:[ a_class [ "heading-2" ] ]
              [ txt (session |> Session.session_date_to_human) ]
          ; Partials.overview_list language assignments
          ])
      assignments
    |> div ~a:[ a_class [ "stack-lg" ] ]
  in
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.NavLink Pool_common.I18n.Assignments)
    experiment.Experiment.id
    ~active:Pool_common.I18n.Assignments
    html
;;
