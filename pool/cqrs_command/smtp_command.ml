module Conformist = Pool_common.Utils.PoolConformist
module SmtpAuth = Pool_tenant.SmtpAuth

let src = Logs.Src.create "smtp.cqrs"

module Create : sig
  type create =
    { label : SmtpAuth.Label.t
    ; server : SmtpAuth.Server.t
    ; port : SmtpAuth.Port.t
    ; username : SmtpAuth.Username.t option
    ; password : SmtpAuth.Password.t option
    ; mechanism : SmtpAuth.Mechanism.t
    ; protocol : SmtpAuth.Protocol.t
    }

  include Common.CommandSig with type t = create

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val handle
    :  ?id:SmtpAuth.Id.t
    -> ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type create =
    { label : SmtpAuth.Label.t
    ; server : SmtpAuth.Server.t
    ; port : SmtpAuth.Port.t
    ; username : SmtpAuth.Username.t option
    ; password : SmtpAuth.Password.t option [@opaque]
    ; mechanism : SmtpAuth.Mechanism.t
    ; protocol : SmtpAuth.Protocol.t
    }

  type t = create

  let command label server port username password mechanism protocol =
    { label; server; port; username; password; mechanism; protocol }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ SmtpAuth.Label.schema ()
          ; SmtpAuth.Server.schema ()
          ; SmtpAuth.Port.schema ()
          ; Conformist.optional @@ SmtpAuth.Username.schema ()
          ; Conformist.optional @@ SmtpAuth.Password.schema ()
          ; SmtpAuth.Mechanism.schema ()
          ; SmtpAuth.Protocol.schema ()
          ]
        command)
  ;;

  let handle ?id ?(tags = Logs.Tag.empty) (command : t) =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    SmtpAuth.Write.create
      ?id
      command.label
      command.server
      command.port
      command.username
      command.password
      command.mechanism
      command.protocol
    >|= fun smtp -> [ Pool_tenant.SmtpCreated smtp |> Pool_event.pool_tenant ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Pool_tenant.Guard.Access.Smtp.create
end

module Update : sig
  include Common.CommandSig with type t = SmtpAuth.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.SmtpAuth.Id.t -> Guard.ValidationSet.t
end = struct
  type t = SmtpAuth.t

  let command id label server port username mechanism protocol =
    { SmtpAuth.id; label; server; port; username; mechanism; protocol }
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ SmtpAuth.Id.schema ()
          ; SmtpAuth.Label.schema ()
          ; SmtpAuth.Server.schema ()
          ; SmtpAuth.Port.schema ()
          ; Conformist.optional @@ SmtpAuth.Username.schema ()
          ; SmtpAuth.Mechanism.schema ()
          ; SmtpAuth.Protocol.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) (command : t) =
    Logs.info ~src (fun m -> m "Handle command Edit" ~tags);
    let update =
      { SmtpAuth.id = command.SmtpAuth.id
      ; label = command.SmtpAuth.label
      ; server = command.SmtpAuth.server
      ; port = command.SmtpAuth.port
      ; username = command.SmtpAuth.username
      ; mechanism = command.SmtpAuth.mechanism
      ; protocol = command.SmtpAuth.protocol
      }
    in
    Ok [ Pool_tenant.SmtpEdited update |> Pool_event.pool_tenant ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Pool_tenant.Guard.Access.Smtp.update
end

module UpdatePassword : sig
  include Common.CommandSig with type t = SmtpAuth.update_password

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.SmtpAuth.Id.t -> Guard.ValidationSet.t
end = struct
  type t = SmtpAuth.update_password

  let command id password = { SmtpAuth.id; password }

  let schema =
    Conformist.(
      make
        Field.
          [ SmtpAuth.Id.schema ()
          ; Conformist.optional @@ SmtpAuth.Password.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) (command : t) =
    Logs.info ~src (fun m -> m "Handle command Edit" ~tags);
    Ok [ Pool_tenant.SmtpPasswordEdited command |> Pool_event.pool_tenant ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Pool_tenant.Guard.Access.Smtp.update
end
