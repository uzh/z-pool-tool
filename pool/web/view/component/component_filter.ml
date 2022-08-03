open Tyxml.Html
module Input = Component_input

let form_action ?path id =
  let base =
    Format.asprintf
      "/api/admin/filter/experiment/%s/create"
      (id |> Pool_common.Id.value)
  in
  CCOption.map_or
    ~default:base
    (fun path -> Format.asprintf "%s/%s" base path)
    path
  |> Sihl.Web.externalize_path
;;

let filter_lit csrf experiment =
  div
    [ h3 ~a:[ a_class [ "heading-3" ] ] [ txt "Filter contacts" ]
    ; div
        ~a:[ a_class [ "gap" ] ]
        [ Unsafe.node
            "contact-filter"
            ~a:
              [ a_user_data "action" (form_action experiment.Experiment.id)
              ; a_user_data "csrf" csrf
              ]
            []
        ]
    ]
;;
