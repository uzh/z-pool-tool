let src = Logs.Src.create "custom_field_answer.cqrs"

module UpdateMultiple : sig
  type t = Custom_field.Public.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_common.Id.t
    -> t
    -> (Pool_event.t, Pool_common.Message.error) result

  val effects : Guard.Authorizer.effect list
end = struct
  type t = Custom_field.Public.t

  let handle ?(tags = Logs.Tag.empty) contact_id f =
    Logs.info ~src (fun m -> m "Handle command UpdateMultiple" ~tags);
    Ok (Custom_field.AnswerUpserted (f, contact_id) |> Pool_event.custom_field)
  ;;

  let effects = [ `Create, `TargetEntity `Admin ]
end
