let src = Logs.Src.create "custom_field_answer.cqrs"

module UpdateMultiple : sig
  include Common.CommandSig with type t = Custom_field.Public.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_context.user
    -> Pool_common.Id.t
    -> t
    -> (Pool_event.t, Pool_common.Message.error) result

  val effects : Custom_field.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Custom_field.Public.t

  let handle ?(tags = Logs.Tag.empty) user contact_id f =
    Logs.info ~src (fun m -> m "Handle command UpdateMultiple" ~tags);
    Ok
      (Custom_field.AnswerUpserted (f, contact_id, user)
       |> Pool_event.custom_field)
  ;;

  let effects id =
    let open Guard in
    let target_id = id |> Guard.Uuid.target_of Custom_field.Id.value in
    ValidationSet.One (Action.Update, TargetSpec.Id (`CustomField, target_id))
  ;;
end
