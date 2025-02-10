open Tyxml.Html
open Component.Input
open Pool_message.Control

let field_of_role = function
  | `Assistants -> Field.Assistants
  | `Experimenter -> Field.Experimenter
;;

let experiment_user_path experiment role =
  let field = field_of_role role in
  HttpUtils.Url.Admin.experiment_user_path experiment.Experiment.id field
;;

let assign_form
      { Pool_context.csrf; language; _ }
      (url : ?admin_id:Admin.Id.t -> ?suffix:uri -> unit -> string)
      action
      admin
  =
  let suffix, control, style =
    match action with
    | `Assign -> "assign", Assign None, `Success
    | `Unassign -> "unassign", Unassign None, `Error
  in
  form
    ~a:
      [ a_action (url ~admin_id:(Admin.id admin) ~suffix () |> Sihl.Web.externalize_path)
      ; a_method `Post
      ]
    [ csrf_element csrf ()
    ; submit_element ~submit_type:style ~classnames:[ "small" ] language control ()
    ]
;;

let list_existing context experiment role ~can_unassign =
  let button_url = experiment_user_path experiment role in
  let query_url =
    HttpUtils.Url.Admin.experiment_user_path
      experiment.Experiment.id
      (field_of_role role)
      ~suffix:"assigned"
      ()
  in
  Page_admin_admins.list
    ~buttons:(if can_unassign then [ assign_form context button_url `Unassign ] else [])
    ~hide_create:true
    ~table_id:"existing-admins"
    ~url:query_url
    context
;;

let list_available context experiment role ~can_assign =
  let url = experiment_user_path experiment role in
  Page_admin_admins.list
    ~buttons:(if can_assign then [ assign_form context url `Assign ] else [])
    ~hide_create:true
    ~table_id:"available-admins"
    context
;;

let role_assignment
      ?hint
      ?(can_assign = false)
      ?(can_unassign = false)
      context
      experiment
      role
      ~applicable:available
      ~current:existing
  =
  let open CCFun in
  (* let open Pool_common.I18n in *)
  let existing = list_existing context experiment role ~can_unassign existing in
  let available = list_available context experiment role ~can_assign available in
  let main_hint =
    CCOption.map_or
      ~default:(txt "")
      (I18n.content_to_string %> Unsafe.data %> CCList.return %> div)
      hint
  in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ main_hint; div ~a:[ a_class [ "switcher"; "flex-gap" ] ] [ existing; available ] ]
;;
