open Tyxml.Html

let to_horizontal_table ?classnames language values =
  let attrs =
    CCOption.map_or
      ~default:[]
      (fun classnames -> [ a_class classnames ])
      classnames
  in
  values
  |> CCList.map (fun (field, value) ->
         tr
           [ th
               [ txt
                   (field
                   |> Pool_common.Utils.field_to_string_capitalized language)
               ]
           ; td [ txt value ]
           ])
  |> table ~a:attrs
;;

let personal_detail language contact =
  let open Contact in
  Pool_common.Message.
    [ Field.Name, fullname contact
    ; Field.Email, email_address contact |> Pool_user.EmailAddress.value
    ]
  |> to_horizontal_table ~classnames:[ "striped" ] language
;;
