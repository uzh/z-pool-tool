module Conformist = Pool_conformist
module SmtpAuth = Email.SmtpAuth

let src = Logs.Src.create "smtp.cqrs"

let clear_cache_event ?id () =
  let open System_event in
  Job.SmtpAccountUpdated |> create ?id |> created |> Pool_event.system_event
;;

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

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result

  val smtp_of_command
    :  ?id:SmtpAuth.Id.t
    -> t
    -> (SmtpAuth.Write.t, Pool_message.Error.t) Result.t

  val handle
    :  ?event_id:System_event.Id.t
    -> ?tags:Logs.Tag.set
    -> SmtpAuth.t option
    -> SmtpAuth.Write.t
    -> (Pool_event.t list, Pool_message.Error.t) result
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

  let smtp_of_command
    ?id
    { label; server; port; username; password; mechanism; protocol; default }
    =
    SmtpAuth.Write.create
      ?id
      label
      server
      port
      username
      password
      mechanism
      protocol
      default
  ;;

  let handle ?event_id ?(tags = Logs.Tag.empty) current_default smtp_auth =
    let open CCResult in
    let open SmtpAuth in
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let is_default =
      (match smtp_auth.Write.default |> Default.value, current_default with
       | (false | true), None -> true
       | true, Some _ -> true
       | false, Some _ -> false)
      |> Default.create
    in
    let smtp = Write.{ smtp_auth with default = is_default } in
    Ok
      [ Email.SmtpCreated smtp |> Pool_event.email
      ; clear_cache_event ?id:event_id ()
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Email.Guard.Access.Smtp.create
end

module Update : sig
  include Common.CommandSig with type t = update

  val handle
    :  ?tags:Logs.Tag.set
    -> ?clear_id:System_event.Id.t
    -> SmtpAuth.t option
    -> SmtpAuth.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : SmtpAuth.Id.t -> Guard.ValidationSet.t
end = struct
  type t = update

  let handle
    ?(tags = Logs.Tag.empty)
    ?(clear_id = System_event.Id.create ())
    (default_smtp : SmtpAuth.t option)
    (smtp_auth : SmtpAuth.t)
    (command : t)
    =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Edit" ~tags);
    let default =
      let open CCFun.Infix in
      let open SmtpAuth in
      let force_default =
        default_smtp
        |> CCOption.map_or ~default:true (id %> Id.equal (id smtp_auth))
      in
      (force_default || Default.value command.default) |> Default.create
    in
    let update =
      { SmtpAuth.id = smtp_auth.SmtpAuth.id
      ; label = command.label
      ; server = command.server
      ; port = command.port
      ; username = command.username
      ; mechanism = command.mechanism
      ; protocol = command.protocol
      ; default
      }
    in
    Ok
      [ Email.SmtpEdited update |> Pool_event.email
      ; clear_cache_event ~id:clear_id ()
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate update_schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Email.Guard.Access.Smtp.update
end

module UpdatePassword : sig
  type t = SmtpAuth.Password.t option

  val handle
    :  ?tags:Logs.Tag.set
    -> SmtpAuth.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : SmtpAuth.Id.t -> Guard.ValidationSet.t
end = struct
  type t = SmtpAuth.Password.t option

  let schema =
    Conformist.(
      make Field.[ Conformist.optional @@ SmtpAuth.Password.schema () ] CCFun.id)
  ;;

  let handle ?(tags = Logs.Tag.empty) (smtp : SmtpAuth.t) (password : t) =
    Logs.info ~src (fun m -> m "Handle command Edit" ~tags);
    let command = SmtpAuth.{ id = smtp.SmtpAuth.id; password } in
    Ok
      [ Email.SmtpPasswordEdited command |> Pool_event.email
      ; clear_cache_event ()
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_message.to_conformist_error
  ;;

  let effects = Email.Guard.Access.Smtp.update
end

module Delete : sig
  include Common.CommandSig with type t = SmtpAuth.Id.t

  val handle
    :  ?tags:Logs.Tag.set
    -> ?clear_id:System_event.Id.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : SmtpAuth.Id.t -> Guard.ValidationSet.t
end = struct
  type t = SmtpAuth.Id.t

  let handle
    ?(tags = Logs.Tag.empty)
    ?(clear_id = System_event.Id.create ())
    (command : t)
    =
    Logs.info ~src (fun m -> m "Handle command Delete" ~tags);
    Ok
      [ Email.SmtpDeleted command |> Pool_event.email
      ; clear_cache_event ~id:clear_id ()
      ]
  ;;

  let effects = Email.Guard.Access.Smtp.update
end
