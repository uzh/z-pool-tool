open CCFun
open Tyxml.Html
open Component_input
open Component_table

let rules_path ?suffix () =
  let default = "/admin/settings/rules/" in
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
    (role : Role.Actor.t)
    =
    let button_form target name submit_type confirm_text =
      form
        ~a:
          [ a_method `Post
          ; a_action (rules_path ~suffix:target ())
          ; a_user_data
              "confirmable"
              (Pool_common.Utils.confirmable_to_string language confirm_text)
          ]
        [ csrf_element csrf ()
        ; submit_element ~submit_type language (name None) ()
        ]
    in
    let buttons =
      let open Pool_common in
      let default = [] in
      let target_button =
        CCOption.map_or
          ~default
          (link_as_button ~icon:Icon.EyeOutline %> CCList.return)
          (target_path role)
      in
      let remove_button =
        if is_edit
        then [ button_form "remove" Message.delete `Error I18n.RemoveRule ]
        else default
      in
      target_button @ remove_button
      |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
    in
    [ txt ([%show: Role.Actor.t] role); buttons ]
  ;;

  let create ?is_edit ({ Pool_context.language; _ } as context) roles =
    let open CCList in
    let open Pool_common.Message in
    let thead = (Field.[ Role ] |> fields_to_txt language) @ [ txt "" ] in
    roles >|= row ?is_edit context |> horizontal_table `Striped ~thead
  ;;
end
