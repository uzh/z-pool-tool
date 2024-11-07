open Tyxml.Html
open Component.Input
open Pool_message
module Partials = Component.Partials
module Url = Page_admin_custom_fields.Url

let option_form
  ?(custom_field_option : Custom_field.SelectOption.t option)
  custom_field
  Pool_context.{ language; csrf; _ }
  tenant_languages
  flash_fetcher
  =
  let open Custom_field in
  let action =
    let field = model custom_field, id custom_field in
    match custom_field_option with
    | None -> Url.Option.create_path field
    | Some o -> Url.Option.detail_path field o.SelectOption.id
  in
  let name_inputs =
    Page_admin_custom_fields.input_by_lang
      ~required:true
      language
      tenant_languages
      flash_fetcher
      custom_field_option
      Field.Name
      (fun lang o ->
         let open CCOption in
         o.SelectOption.name
         |> Name.find_opt lang
         >|= Name.value_name
         |> value ~default:"")
  in
  form
    ~a:
      [ a_method `Post
      ; a_action (Sihl.Web.externalize_path action)
      ; a_class [ "stack" ]
      ]
    [ csrf_element csrf ()
    ; div ~a:[ a_class [ "stack" ] ] name_inputs
    ; div
        ~a:[ a_class [ "flexrow" ] ]
        [ submit_element
            ~classnames:[ "push" ]
            language
            Control.(
              let field = Some Field.CustomFieldOption in
              match custom_field_option with
              | None -> Create field
              | Some _ -> Update field)
            ~submit_type:`Primary
            ()
        ]
    ]
;;

let field_buttons language csrf custom_field option =
  let open Custom_field in
  let open Pool_common in
  let action option appendix =
    Url.Option.detail_path
      (model custom_field, custom_field |> id)
      option.SelectOption.id
    |> (fun base -> Format.asprintf "%s/%s" base appendix)
    |> Sihl.Web.externalize_path
  in
  let make_form action msg submit_type confirmable =
    form
      ~a:
        [ a_action action
        ; a_method `Post
        ; a_user_data
            "confirmable"
            (Pool_common.Utils.confirmable_to_string language confirmable)
        ]
      [ csrf_element csrf (); submit_element language msg ~submit_type () ]
  in
  match option with
  | None -> txt ""
  | Some option ->
    (match option.SelectOption.published_at with
     | Some published_at ->
       div
         [ txt
             (Utils.field_to_string language Field.PublishedAt
              |> CCString.capitalize_ascii)
         ; txt ": "
         ; txt
             (published_at
              |> PublishedAt.value
              |> Pool_model.Time.formatted_date_time)
         ]
     | None ->
       div
         ~a:[ a_class [ "flexrow"; "flex-gap"; "justify-end" ] ]
         [ make_form
             (action option "delete")
             Control.(Delete (Some Field.CustomFieldOption))
             `Error
             I18n.DeleteCustomFieldOption
         ; make_form
             (action option "publish")
             Control.(Publish (Some Field.CustomFieldOption))
             `Success
             I18n.PublishCustomFieldOption
         ])
;;

let detail
  ?custom_field_option
  custom_field
  (Pool_context.{ language; csrf; _ } as context)
  sys_languages
  flash_fetcher
  =
  let changelog_html =
    let open Custom_field in
    match custom_field_option with
    | None -> txt ""
    | Some option ->
      let model = model custom_field in
      let url =
        HttpUtils.Url.Admin.custom_field_option_path
          model
          (id custom_field)
          ~suffix:"changelog"
          ~id:option.SelectOption.id
          ()
        |> Uri.of_string
      in
      Component.Changelog.list context url None
  in
  let buttons_form =
    field_buttons language csrf custom_field custom_field_option
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ Partials.form_title language Field.CustomFieldOption custom_field_option
    ; div
        ~a:[ a_class [ "stack-lg" ] ]
        [ buttons_form
        ; option_form
            ?custom_field_option
            custom_field
            context
            sys_languages
            flash_fetcher
        ; changelog_html
        ]
    ]
;;
