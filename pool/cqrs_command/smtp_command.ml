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
    ; authentication_method : SmtpAuth.AuthenticationMethod.t
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
    ; password : SmtpAuth.Password.t option
    ; authentication_method : SmtpAuth.AuthenticationMethod.t
    ; protocol : SmtpAuth.Protocol.t
    }

  type t = create

  let command label server port username password authentication_method protocol
    =
    { label; server; port; username; password; authentication_method; protocol }
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
          ; SmtpAuth.AuthenticationMethod.schema ()
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
      command.authentication_method
      command.protocol
    >|= fun smtp -> [ Pool_tenant.SmtpCreated smtp |> Pool_event.pool_tenant ]
  ;;

  let effects = [ `Create, `TargetEntity `Smtp ]

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;
end

module Edit : sig
  include Common.CommandSig with type t = SmtpAuth.t

  val handle
    :  ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.SmtpAuth.Id.t -> Guard.Authorizer.effect list
end = struct
  type t = SmtpAuth.t

  let command id label server port username authentication_method protocol =
    { SmtpAuth.id
    ; label
    ; server
    ; port
    ; username
    ; authentication_method
    ; protocol
    }
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
          ; SmtpAuth.AuthenticationMethod.schema ()
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
      ; authentication_method = command.SmtpAuth.authentication_method
      ; protocol = command.SmtpAuth.protocol
      }
    in
    Ok [ Pool_tenant.SmtpEdited update |> Pool_event.pool_tenant ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Pool_tenant.SmtpAuth.Id.value)
    ; `Update, `TargetEntity `Tenant
    ]
  ;;
end

module UpdatePassword : sig
  include Common.CommandSig with type t = SmtpAuth.update_password

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Pool_tenant.SmtpAuth.Id.t -> Guard.Authorizer.effect list
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

  let effects id =
    [ `Update, `Target (id |> Guard.Uuid.target_of Pool_tenant.SmtpAuth.Id.value)
    ; `Update, `TargetEntity `Smtp
    ]
  ;;
end
