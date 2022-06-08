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

  let canceled_at (a : Assignment.t) =
    a.canceled_at
    |> Assignment.CanceledAt.value
    |> CCOption.map_or ~default:"" (fun c ->
           Pool_common.Utils.Time.formatted_date_time c)
    |> txt
  ;;

  let overview_list Pool_context.{ language; csrf; _ } experiment_id assignments
    =
    let action assignment =
      let value = Pool_common.Id.value in
      Format.asprintf
        "/admin/experiments/%s/assignments/%s/cancel"
        (experiment_id |> value)
        (assignment.Assignment.id |> value)
      |> Sihl.Web.externalize_path
    in
    match CCList.is_empty assignments with
    | true -> p [ language |> empty ]
    | false ->
      let thead =
        Pool_common.Message.Field.
          [ Some Name; Some Email; Some CanceledAt; None ]
        |> Component.Table.head language
      in
      CCList.map
        (fun (assignment : Assignment.t) ->
          let base =
            [ td [ assignment |> contact_fullname ]
            ; td [ assignment |> contact_email ]
            ; td [ assignment |> canceled_at ]
            ]
          in
          let cancel assignment =
            form
              ~a:[ a_action (action assignment); a_method `Post ]
              [ Component.csrf_element csrf ()
              ; Component.submit_element
                  language
                  (Pool_common.Message.Cancel None)
                  ~submit_type:`Error
                  ()
              ]
          in
          (match assignment.canceled_at |> Assignment.CanceledAt.value with
          | None -> base @ [ td [ cancel assignment ] ]
          | Some _ -> base @ [ td [] ])
          |> tr)
        assignments
      |> table ~thead ~a:[ a_class [ "striped" ] ]
  ;;
end

let list assignments experiment (Pool_context.{ language; _ } as context) =
  let html =
    CCList.map
      (fun (session, assignments) ->
        div
          [ h2
              ~a:[ a_class [ "heading-2" ] ]
              [ txt (session |> Session.session_date_to_human) ]
          ; Partials.overview_list context experiment.Experiment.id assignments
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
