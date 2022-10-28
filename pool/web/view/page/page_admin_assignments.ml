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
    |> CCOption.map_or ~default:"" (fun c ->
         c
         |> Assignment.CanceledAt.value
         |> Pool_common.Utils.Time.formatted_date_time)
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
      in
      let rows =
        CCList.map
          (fun (assignment : Assignment.t) ->
            let base =
              [ assignment |> contact_fullname
              ; assignment |> contact_email
              ; assignment |> canceled_at
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
            match assignment.canceled_at with
            | None -> base @ [ cancel assignment ]
            | Some _ -> base @ [ txt "" ])
          assignments
      in
      Component.Table.horizontal_table `Striped language ~thead rows
  ;;
end

let list assignments experiment (Pool_context.{ language; _ } as context) =
  let html =
    CCList.map
      (fun (session, assignments) ->
        div
          [ h3
              ~a:[ a_class [ "heading-3" ] ]
              [ txt (session |> Session.session_date_to_human) ]
          ; Partials.overview_list context experiment.Experiment.id assignments
          ])
      assignments
    |> div ~a:[ a_class [ "stack-lg" ] ]
  in
  Page_admin_experiments.experiment_layout
    language
    (Page_admin_experiments.NavLink Pool_common.I18n.Assignments)
    experiment
    ~active:Pool_common.I18n.Assignments
    html
;;
