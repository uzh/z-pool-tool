open CCFun
open Tyxml.Html
open Component_input
open Component_table

let roles_path ?suffix admin =
  let default =
    Format.asprintf "/admin/admins/%s/" Admin.(id admin |> Id.value)
  in
  CCOption.map_or ~default (Format.asprintf "%s%s" default) suffix
;;

let target_path : Role.Actor.t -> string option =
  let open Guard.Uuid.Target in
  let experiment_path = to_string %> Format.asprintf "/admin/experiments/%s/" in
  let location_path = to_string %> Format.asprintf "/admin/locations/%s/" in
  function
  | `Assistant id -> Some (experiment_path id)
  | `Experimenter id -> Some (experiment_path id)
  | `LocationManager id -> Some (location_path id)
  | `ManageAssistant id -> Some (experiment_path id)
  | `ManageExperimenter id -> Some (experiment_path id)
  | `Recruiter id -> Some (experiment_path id)
  | _ -> None
;;

module List = struct
  let row
    ?(is_edit = false)
    Pool_context.{ csrf; language; _ }
    target_admin
    (role : Role.Actor.t)
    =
    let button_form target name submit_type confirm_text =
      form
        ~a:
          [ a_method `Post
          ; a_action (roles_path ~suffix:target target_admin)
          ; a_user_data
              "confirmable"
              (Pool_common.Utils.confirmable_to_string language confirm_text)
          ]
        [ csrf_element csrf ()
        ; submit_element ~submit_type language (name None) ()
        ; input
            ~a:
              [ a_name Field.(show Role)
              ; a_value ([%show: Role.Actor.t] role)
              ; a_hidden ()
              ]
            ()
        ]
    in
    let buttons =
      let open Pool_common in
      let default = [] in
      let target_button =
        CCOption.map_or
          ~default
          (link_as_button ~icon:Icon.Eye %> CCList.return)
          (target_path role)
      in
      let remove_button =
        if is_edit
        then [ button_form "revoke-role" Message.delete `Error I18n.RevokeRole ]
        else default
      in
      target_button @ remove_button
      |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
    in
    [ txt ([%show: Role.Actor.t] role); buttons ]
  ;;

  let create
    ?is_edit
    ({ Pool_context.language; _ } as context)
    target_admin
    roles
    =
    let open CCList in
    let open Pool_common.Message in
    let thead = (Field.[ Role ] |> fields_to_txt language) @ [ txt "" ] in
    roles
    >|= row ?is_edit context target_admin
    |> horizontal_table `Striped ~thead
  ;;
end
