open Tyxml.Html

let formatted_address language address =
  let open Pool_location.Address in
  (match address with
   | Virtual ->
     Pool_common.Utils.field_to_string
       language
       Pool_common.Message.Field.Virtual
     |> CCString.capitalize_ascii
     |> CCList.pure
   | Physical Mail.{ institution; room; building; street; zip; city } ->
     let open Mail in
     let open CCOption in
     [ institution >|= Institution.value
     ; room |> Room.value |> pure
     ; building >|= Building.value
     ; street |> Street.value |> pure
     ; Format.asprintf "%s %s" (Zip.value zip) (City.value city) |> pure
     ]
     |> CCList.filter_map CCFun.id)
  |> CCList.fold_left
       (fun acc curr ->
         match acc with
         | [] -> [ txt curr ]
         | _ -> acc @ [ br (); txt curr ])
       []
;;

let preview (location : Pool_location.t) =
  let open Pool_location in
  let name = p [ txt (Name.value location.name) ] in
  let link =
    let url =
      Format.asprintf
        "%s/%s"
        Pool_common.Message.Field.(human_url Location)
        (Id.value location.id)
      |> Sihl.Web.externalize_path
    in
    a ~a:[ a_href url ] [ txt "Details" ]
  in
  [ name; br (); link ] |> address
;;
