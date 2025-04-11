open Tyxml.Html
open Component.Location

let show { Pool_context.language; _ } (location : Pool_location.t) files =
  let open Pool_location in
  let files_html =
    match files with
    | [] -> txt ""
    | files ->
      let open Mapping in
      div
        [ h2
            ~a:[ a_class [ "has-gap" ] ]
            [ txt Pool_common.(Utils.text_to_string language I18n.Files) ]
        ; CCList.map
            (fun (mapping : file) ->
               let label =
                 Format.asprintf
                   "%s (%s)"
                   (mapping.label |> Label.show |> CCString.capitalize_ascii)
                   (mapping.language |> Pool_common.Language.show)
               in
               let path =
                 contact_file_path location.id mapping |> Sihl.Web.externalize_path
               in
               [ a ~a:[ a_href path ] [ txt label ] ])
            files
          |> Component.Table.horizontal_table `Striped
        ]
  in
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ h1 ~a:[ a_class [ "heading-1"; "has-gap" ] ] [ txt (Name.value location.name) ]
    ; div
        ~a:[ a_class [ "grid-col-2"; "grid-gap-lg" ] ]
        [ div
            [ h2
                [ Pool_common.(Utils.text_to_string language I18n.Address)
                  |> CCString.capitalize_ascii
                  |> txt
                ]
            ; formatted_address language location.address |> div
            ; location.link
              |> CCOption.map_or ~default:(txt "") (fun link ->
                p
                  ~a:[ a_class [ "gap-sm" ] ]
                  [ a
                      ~a:[ a_href (Link.value link); a_target "_blank" ]
                      [ Pool_common.Utils.control_to_string
                          language
                          Pool_message.Control.More
                        |> txt
                      ]
                  ])
            ]
        ; files_html
        ]
    ; CCOption.bind location.description (fun description ->
        description
        |> Description.find_opt language
        |> CCOption.map (fun desc ->
          desc |> Unsafe.data |> CCList.return |> div ~a:[ a_class [ "gap-lg" ] ]))
      |> CCOption.value ~default:(txt "")
    ]
;;
