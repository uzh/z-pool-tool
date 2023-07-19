open CCFun
open Tyxml.Html
module HttpUtils = Http_utils
module Input = Component.Input
module Field = Pool_common.Message.Field

let tags_path ?suffix () =
  let default = "/admin/settings/tags/" in
  CCOption.map_or ~default (Format.asprintf "%s%s" default) suffix
  |> Sihl.Web.externalize_path
;;

let layout language children =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    (h1
       ~a:[ a_class [ "heading-1" ] ]
       [ txt Pool_common.(Utils.nav_link_to_string language I18n.Tags) ]
     :: children)
;;

module List = struct
  let row ({ Tags.title; description; _ } as tag) =
    let buttons tag =
      tags_path ~suffix:(tag.Tags.id |> Tags.Id.value) ()
      |> Input.edit_link
      |> CCList.return
      |> div ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
    in
    [ txt (Tags.Title.value title)
    ; txt (CCOption.map_or ~default:"" Tags.Description.value description)
    ; buttons tag
    ]
  ;;

  let create { Pool_context.language; _ } tags =
    let thead =
      let open Input in
      (Field.[ Title; Description ] |> Component.Table.fields_to_txt language)
      @ [ link_as_button
            ~style:`Success
            ~icon:Icon.Add
            ~control:(language, Pool_common.Message.(Add (Some Field.Tag)))
            (tags_path ~suffix:"create" ())
        ]
    in
    CCList.map row tags
    |> Component.Table.horizontal_table `Striped ~thead ~align_last_end:true
  ;;
end

let index ({ Pool_context.language; _ } as context) tags =
  layout
    language
    [ p
        [ Pool_common.(Utils.hint_to_string language I18n.TagsIntro)
          |> HttpUtils.add_line_breaks
        ]
    ; List.create context tags
    ]
;;

let tag_form ?flash_fetcher ?tag Pool_context.{ language; csrf; _ } =
  let open Tags in
  let action =
    match tag with
    | None -> tags_path ()
    | Some tag -> tags_path ~suffix:(tag.id |> Id.value) ()
  in
  form
    ~a:
      [ a_method `Post
      ; a_action (Sihl.Web.externalize_path action)
      ; a_class [ "stack" ]
      ; a_user_data "detect-unsaved-changes" ""
      ]
    [ Input.csrf_element csrf ()
    ; div
        ~a:[ a_class [ "grid-col-2" ] ]
        [ Input.input_element
            language
            `Text
            Field.Title
            ?value:(CCOption.map (fun m -> m.title |> Title.value) tag)
            ?flash_fetcher
            ~required:true
        ; Input.input_element
            language
            `Text
            Field.Description
            ?value:
              CCOption.(bind tag (fun m -> m.description) >|= Description.value)
            ?flash_fetcher
        ]
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ div
            ~a:[ a_class [ "push"; "flexrow"; "flex-gap-lg" ] ]
            [ Input.reset_form_button language
            ; Input.submit_element
                language
                Pool_common.Message.(
                  let field = Some Field.Tag in
                  match tag with
                  | None -> Create field
                  | Some _ -> Update field)
                ~submit_type:`Primary
                ()
            ]
        ]
    ]
;;

let edit ?flash_fetcher ({ Pool_context.language; _ } as context) tag =
  [ div ~a:[ a_class [ "stack-lg" ] ] [ tag_form ?flash_fetcher ~tag context ] ]
  |> layout language
;;

let new_form ?flash_fetcher ({ Pool_context.language; _ } as context) =
  [ div ~a:[ a_class [ "stack-lg" ] ] [ tag_form ?flash_fetcher context ] ]
  |> layout language
;;
