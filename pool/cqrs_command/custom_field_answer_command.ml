module UpdateMultiple : sig
  type t = (string * Custom_field.Public.t) list

  val handle
    :  Pool_common.Id.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Ocauth.Authorizer.effect list
end = struct
  type t = (string * Custom_field.Public.t) list

  let handle contact_id lst =
    let open CCResult in
    CCList.map
      (fun (value, field) ->
        Custom_field.Public.validate value field
        >|= fun e ->
        Custom_field.AnswerUpserted (e, contact_id) |> Pool_event.custom_field)
      lst
    |> CCList.all_ok
  ;;

  let effects = [ `Create, `Role `Admin ]
end
