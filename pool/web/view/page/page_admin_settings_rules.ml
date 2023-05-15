open CCFun
open Tyxml.Html
module HttpUtils = Http_utils
module Input = Component.Input

let rules_path ?suffix () =
  let default = "/admin/settings/rules/" in
  CCOption.map_or ~default (Format.asprintf "%s%s" default) suffix
;;

module List = struct
  let row
    Pool_context.{ csrf; language; _ }
    ((actor_spec, action, target_spec) : Guard.Rule.t)
    =
    let actor_spec_to_string = function
      | Guard.ActorSpec.Id (role, uuid) ->
        CCString.concat
          ": "
          [ [%show: Role.Actor.t] role; [%show: Guard.Uuid.Actor.t] uuid ]
      | Guard.ActorSpec.Entity role -> [%show: Role.Actor.t] role
    in
    let target_spec_to_string = function
      | Guard.TargetSpec.Id (role, uuid) ->
        CCString.concat
          ": "
          [ [%show: Role.Target.t] role; [%show: Guard.Uuid.Target.t] uuid ]
      | Guard.TargetSpec.Entity role -> [%show: Role.Target.t] role
    in
    let button_form target name submit_type confirm_text =
      form
        ~a:
          [ a_method `Post
          ; a_action (rules_path ~suffix:target ())
          ; a_user_data
              "confirmable"
              (Pool_common.Utils.confirmable_to_string language confirm_text)
          ]
        [ Input.csrf_element csrf ()
        ; Input.submit_element ~submit_type language (name None) ()
        ]
    in
    let buttons =
      Pool_common.[ button_form "remove" Message.delete `Error I18n.RemoveRule ]
      |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
    in
    [ txt (actor_spec_to_string actor_spec)
    ; txt ([%show: Guard.Action.t] action)
    ; txt (target_spec_to_string target_spec)
    ; buttons
    ]
  ;;

  let create ({ Pool_context.language; _ } as context) rules =
    let open Pool_common in
    let thead =
      (Message.Field.[ ActorSpec; Action; TargetSpec ]
       |> Component.Table.fields_to_txt language)
      @ [ txt "" ]
    in
    CCList.map (row context) rules
    |> Component.Table.horizontal_table `Striped ~thead
  ;;
end

let index ({ Pool_context.language; _ } as context) rules =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1
        ~a:[ a_class [ "heading-1" ] ]
        [ txt Pool_common.(Utils.nav_link_to_string language I18n.Rules) ]
    ; p
        [ Pool_common.(Utils.hint_to_string language I18n.RulesIntro)
          |> HttpUtils.add_line_breaks
        ]
    ; List.create context rules
    ]
;;
