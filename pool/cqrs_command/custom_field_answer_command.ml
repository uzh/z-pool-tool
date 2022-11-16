module UpdateMultiple : sig
  type t = Custom_field.Public.t

  val handle
    :  Pool_common.Id.t
    -> t
    -> (Pool_event.t, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  type t = Custom_field.Public.t

  let handle contact_id f =
    Ok (Custom_field.AnswerUpserted (f, contact_id) |> Pool_event.custom_field)
  ;;

  let effects = [ `Create, `TargetEntity `Admin ]
end
