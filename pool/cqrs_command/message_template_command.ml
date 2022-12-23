module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "message_template.cqrs"

let update_command email_subject email_text sms_text =
  Message_template.{ email_subject; email_text; sms_text }
;;

let update_schema =
  let open Message_template in
  Pool_common.Utils.PoolConformist.(
    make
      Field.[ EmailSubject.schema (); EmailText.schema (); SmsText.schema () ]
      update_command)
;;

module Update : sig
  include Common.CommandSig with type t = Message_template.update

  val handle
    :  ?tags:Logs.Tag.set
    -> Message_template.t
    -> t
    -> (Pool_event.t list, 'a) result

  val decode
    :  Conformist.input
    -> (Message_template.update, Pool_common.Message.error) result

  val effects : Message_template.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = Message_template.update

  let handle ?(tags = Logs.Tag.empty) template command =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    Ok
      Message_template.
        [ Updated (template, command) |> Pool_event.message_template ]
  ;;

  let decode data =
    Conformist.decode_and_validate update_schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Message_template.Id.value)
    ; `Update, `TargetEntity `MessageTemplate
    ]
  ;;
end

module RestoreDefault : sig
  type t = Pool_tenant.t

  val handle
    :  Message_template.t list
    -> unit
    -> (Pool_event.t list, Pool_common.Message.error) result

  val effects : Pool_tenant.t -> Guard.Authorizer.effect list
end = struct
  type t = Pool_tenant.t

  let handle default () =
    Ok
      [ Message_template.(DefaultRestored default)
        |> Pool_event.message_template
      ]
  ;;

  let effects tenant =
    [ ( `Delete
      , `Target
          (tenant |> Pool_tenant.id |> Guard.Uuid.target_of Pool_common.Id.value)
      )
    ]
  ;;
end
