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

let text_to_string language i18n =
  Pool_common.Utils.text_to_string language i18n |> CCString.capitalize_ascii
;;

let assign_form
      { Pool_context.csrf; language; _ }
      (url : ?admin_id:Admin.Id.t -> ?suffix:uri -> unit -> string)
      ?(disabled = false)
      role
      action
      admin
  =
  let disabled_hint =
    Pool_common.(
      I18n.HasGlobalRole Role.Role.(name (role :> t)) |> Utils.text_to_string language)
  in
  let suffix, control, style =
    match action with
    | `Assign -> "assign", Assign None, `Success
    | `Unassign -> "unassign", Unassign None, `Error
  in
  let button_attrs = if disabled then [ a_disabled (); a_title disabled_hint ] else [] in
  form
    ~a:
      [ a_action (url ~admin_id:(Admin.id admin) ~suffix () |> Sihl.Web.externalize_path)
      ; a_method `Post
      ]
    [ csrf_element csrf ()
    ; submit_element
        ~attributes:button_attrs
        ~submit_type:style
        ~classnames:[ "small" ]
        language
        control
        ()
    ]
;;

let admin_list
      ~can_submit
      ?(table_id = "admin-list")
      ~(url : ?admin_id:Admin.Id.t -> ?suffix:string -> unit -> string)
      ~role
      ?push_url
      (Pool_context.{ language; _ } as context)
      action
      ((admins, query) : (Admin.t * bool) list_wrap * Query.t)
  =
  let data_table_url =
    (fun suffix -> url ~suffix () |> Uri.of_string)
    @@
    match action with
    | `Assign -> "available"
    | `Unassign -> "assigned"
  in
  let data_table =
    Component.DataTable.create_meta
      ~search:Contact.searchable_by
      ?push_url
      data_table_url
      query
      language
  in
  let cols = Page_admin_admins.List.cols ~hide_create:true language in
  let th_class = [ "w-5"; "w-5"; "w-2" ] in
  let row (admin, disabled) =
    let assign_form =
      assign_form ~disabled:(disabled || not can_submit) context url role action admin
    in
    Page_admin_admins.List.row ~additional_buttons:[ assign_form ] language admin
  in
  Component.DataTable.make
    ~break_mobile:true
    ~th_class
    ~target_id:table_id
    ~cols
    ~row
    data_table
    admins
;;

let list_existing context url ~can_unassign ~role admins =
  admin_list
    ~can_submit:can_unassign
    ~table_id:"existing-admins"
    ~url
    ~role
    ~push_url:false
    context
    `Unassign
    admins
;;

let list_available context url ~can_assign ~role admins =
  admin_list
    ~can_submit:can_assign
    ~table_id:"available-admins"
    ~url
    ~role
    ~push_url:false
    context
    `Assign
    admins
;;

let role_assignment
      ?hint
      ?(can_assign = false)
      ?(can_unassign = false)
      ~role
      context
      form_path
      ~applicable:available
      ~current:existing
  =
  let open CCFun in
  let existing =
    div
      ~a:[ a_class [ "stack" ] ]
      [ h3
          [ txt (text_to_string context.Pool_context.language Pool_common.I18n.Assigned) ]
      ; list_existing context form_path ~role ~can_unassign existing
      ]
  in
  let available =
    div
      ~a:[ a_class [ "stack" ] ]
      [ h3
          [ txt (text_to_string context.Pool_context.language Pool_common.I18n.Available)
          ]
      ; list_available context form_path ~role ~can_assign available
      ]
  in
  let main_hint =
    CCOption.map_or
      ~default:(txt "")
      (I18n.content_to_string %> Unsafe.data %> CCList.return %> div)
      hint
  in
  div
    ~a:[ a_class [ "stack-lg" ] ]
    [ main_hint; div ~a:[ a_class [ "grid-col-2"; "flex-gap" ] ] [ existing; available ] ]
;;
