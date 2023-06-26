module Conformist = Pool_common.Utils.PoolConformist
module SmtpAuth = Email.SmtpAuth

let src = Logs.Src.create "smtp.cqrs"

type create =
  { label : SmtpAuth.Label.t
  ; server : SmtpAuth.Server.t
  ; port : SmtpAuth.Port.t
  ; username : SmtpAuth.Username.t option
  ; password : SmtpAuth.Password.t option
  ; mechanism : SmtpAuth.Mechanism.t
  ; protocol : SmtpAuth.Protocol.t
  ; default : SmtpAuth.Default.t
  }

type update =
  { label : SmtpAuth.Label.t
  ; server : SmtpAuth.Server.t
  ; port : SmtpAuth.Port.t
  ; username : SmtpAuth.Username.t option
  ; mechanism : SmtpAuth.Mechanism.t
  ; protocol : SmtpAuth.Protocol.t
  ; default : SmtpAuth.Default.t
  }

let update_command label server port username mechanism protocol default =
  { label; server; port; username; mechanism; protocol; default }
;;

let update_schema =
  Conformist.(
    make
      Field.
        [ SmtpAuth.Label.schema ()
        ; SmtpAuth.Server.schema ()
        ; SmtpAuth.Port.schema ()
        ; Conformist.optional @@ SmtpAuth.Username.schema ()
        ; SmtpAuth.Mechanism.schema ()
        ; SmtpAuth.Protocol.schema ()
        ; SmtpAuth.Default.schema ()
        ]
      update_command)
;;

module Create : sig
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
  type t = create

  let command label server port username password mechanism protocol default =
    { label; server; port; username; password; mechanism; protocol; default }
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
          ; SmtpAuth.Default.schema ()
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
      command.default
    >|= fun smtp -> [ Email.SmtpCreated smtp |> Pool_event.email ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Email.Guard.Access.Smtp.create
end

module Update : sig
  include Common.CommandSig with type t = update

  val handle
    :  ?tags:Logs.Tag.set
    -> SmtpAuth.t
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : SmtpAuth.Id.t -> Guard.ValidationSet.t
end = struct
  type t = update

  let handle ?(tags = Logs.Tag.empty) (smtp_auth : SmtpAuth.t) (command : t) =
    Logs.info ~src (fun m -> m "Handle command Edit" ~tags);
    let update =
      { SmtpAuth.id = smtp_auth.SmtpAuth.id
      ; label = command.label
      ; server = command.server
      ; port = command.port
      ; username = command.username
      ; mechanism = command.mechanism
      ; protocol = command.protocol
      ; default = command.default
      }
    in
    Ok [ Email.SmtpEdited update |> Pool_event.email ]
  ;;

  let decode data =
    Conformist.decode_and_validate update_schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Email.Guard.Access.Smtp.update
end

module UpdatePassword : sig
  include Common.CommandSig with type t = SmtpAuth.update_password

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : SmtpAuth.Id.t -> Guard.ValidationSet.t
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
    Ok [ Email.SmtpPasswordEdited command |> Pool_event.email ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Email.Guard.Access.Smtp.update
end
