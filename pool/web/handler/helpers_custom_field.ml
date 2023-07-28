let answer_and_validate_multiple
  req
  urlencoded
  language
  (custom_fields : Custom_field.Public.t list)
  =
  let open Utils.Lwt_result.Infix in
  let open Custom_field in
  Lwt_list.map_s
    (fun (field : Public.t) ->
      let id = field |> Public.id |> Id.value in
      (match field with
       | Public.MultiSelect _ ->
         req
         |> Http_utils.htmx_urlencoded_list
              (field
               |> Public.to_common_field language
               |> Pool_common.Message.Field.array_key)
       | Public.Boolean _
       | Public.Date _
       | Public.Number _
       | Public.Select _
       | Public.Text _ ->
         CCList.assoc_opt ~eq:CCString.equal id urlencoded
         |> CCOption.value ~default:[]
         |> Lwt.return)
      ||> fun value -> (Custom_field.validate_htmx ~is_admin:false value) field)
    custom_fields
  ||> CCList.all_ok
;;
