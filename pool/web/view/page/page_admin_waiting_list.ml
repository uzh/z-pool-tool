open Tyxml.Html
open Component
module Message = Pool_common.Message

let detail
    (Waiting_list.{ id; contact; experiment; comment; _ } : Waiting_list.t)
    Pool_context.{ language; _ }
  =
  div
    ~a:[ a_class [ "trim"; "safety-margin" ] ]
    [ div
        ~a:[ a_class [ "stack" ] ]
        [ div
            [ h2
                ~a:[ a_class [ "heading-2" ] ]
                [ txt
                    Pool_common.(
                      Utils.field_to_string_capitalized
                        language
                        Message.Field.Contact)
                ]
            ; Page_admin_contact.personal_detail language contact
            ]
        ; form
            ~a:
              [ a_class [ "stack" ]
              ; a_method `Post
              ; a_action
                  (let open Pool_common.Id in
                  Sihl.Web.externalize_path
                    (Format.asprintf
                       "/admin/experiments/%s/waiting-list/%s"
                       (value experiment.Experiment.id)
                       (value id)))
              ]
            [ textarea_element
                language
                Pool_common.Message.Field.(show Comment)
                Pool_common.Message.Field.Comment
                (CCOption.map_or ~default:"" Waiting_list.Comment.value comment)
                ()
            ; submit_element
                language
                Pool_common.Message.(Save (Some Field.Comment))
                ()
            ]
        ]
    ]
;;
