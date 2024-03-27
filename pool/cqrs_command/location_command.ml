module Conformist = Pool_conformist
module Message = Pool_message
module BaseGuard = Guard
open Pool_location

let src = Logs.Src.create "location.cqrs"

module Create : sig
  include Common.CommandSig

  type address = Address.t

  type t =
    { name : Name.t
    ; description : Description.t option
    ; link : Link.t option
    ; address : address
    }

  val handle
    :  ?tags:Logs.Tag.set
    -> ?id:Id.t
    -> t
    -> (Pool_event.t list, 'a) result

  val decode
    :  Description.t option
    -> Conformist.input
    -> (t, Pool_message.Error.t) result
end = struct
  type base =
    { name : Name.t
    ; link : Link.t option
    }

  type address = Address.t

  type t =
    { name : Name.t
    ; description : Description.t option
    ; link : Link.t option
    ; address : address
    }

  let command_base name link = { name; link }
  let schema_mail_address = Address.Mail.schema ()

  let schema =
    Conformist.(
      make
        Field.[ Name.schema (); Conformist.optional @@ Link.schema () ]
        command_base)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    ?(id = Id.create ())
    ({ name; description; link; address } : t)
    =
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    let open CCResult in
    let location =
      { id
      ; name
      ; description
      ; address
      ; link
      ; status = Status.Active
      ; files = []
      ; created_at = Pool_common.CreatedAt.create ()
      ; updated_at = Pool_common.UpdatedAt.create ()
      }
    in
    Ok [ Created location |> Pool_event.pool_location ]
  ;;

  let decode description data =
    let open CCResult in
    map_err Message.to_conformist_error
    @@ let* base = Conformist.decode_and_validate schema data in
       let* address =
         match
           CCList.assoc ~eq:( = ) Pool_message.Field.(Virtual |> show) data
           |> CCList.hd
           |> CCString.equal "true"
         with
         | true -> Ok Address.Virtual
         | false ->
           Conformist.decode_and_validate schema_mail_address data
           >|= Address.physical
       in
       Ok { name = base.name; description; link = base.link; address }
  ;;

  let effects = Pool_location.Guard.Access.create
end

module Update : sig
  include Common.CommandSig

  type t =
    { name : Name.t
    ; link : Link.t option
    ; status : Status.t
    }

  type address = Address.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_location.t
    -> update
    -> (Pool_event.t list, 'a) result

  val decode
    :  Description.t option
    -> Conformist.input
    -> (update, Pool_message.Error.t) result

  val effects : Id.t -> BaseGuard.ValidationSet.t
end = struct
  type t =
    { name : Name.t
    ; link : Link.t option
    ; status : Status.t
    }

  type address = Address.t

  let command_base name link status = { name; link; status }
  let schema_mail_address = Address.Mail.schema ()

  let schema =
    Conformist.(
      make
        Field.
          [ Name.schema ()
          ; Conformist.optional @@ Link.schema ()
          ; Status.schema ()
          ]
        command_base)
  ;;

  let handle ?(tags = Logs.Tag.empty) location update =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    Ok [ Updated (location, update) |> Pool_event.pool_location ]
  ;;

  let decode description data =
    let open CCResult in
    map_err Message.to_conformist_error
    @@ let* base = Conformist.decode_and_validate schema data in
       let* address_new =
         match
           CCList.assoc ~eq:( = ) Pool_message.Field.(Virtual |> show) data
           |> CCList.hd
           |> CCString.equal "true"
         with
         | true -> Ok Address.Virtual
         | false ->
           Conformist.decode_and_validate schema_mail_address data
           >|= Address.physical
       in
       Ok
         { name = base.name
         ; description
         ; address = address_new
         ; link = base.link
         ; status = base.status
         }
  ;;

  let effects id = Pool_location.Guard.Access.update id
end

module AddFile : sig
  include Common.CommandSig with type t = Mapping.file_base

  val handle
    :  ?tags:Logs.Tag.set
    -> Pool_location.t
    -> t
    -> (Pool_event.t list, 'a) result

  val decode : Conformist.input -> (t, Pool_message.Error.t) result
  val effects : Id.t -> BaseGuard.ValidationSet.t
end = struct
  open Mapping

  type t = file_base

  let command label language asset_id = { label; language; asset_id }

  let schema =
    Conformist.(
      make
        Field.
          [ Label.schema ()
          ; Pool_common.Language.schema ()
          ; Pool_conformist.schema_decoder
              CCFun.(Id.of_string %> CCResult.return)
              Id.value
              Pool_message.Field.FileMapping
          ]
        command)
  ;;

  let handle
    ?(tags = Logs.Tag.empty)
    location
    ({ label; language; asset_id } : t)
    =
    Logs.info ~src (fun m -> m "Handle command AddFile" ~tags);
    let open CCResult in
    let file =
      Mapping.Write.create
        label
        language
        (Id.to_common asset_id)
        (location.Pool_location.id
         |> Pool_location.Id.value
         |> Pool_common.Id.of_string)
    in
    Ok [ FileUploaded file |> Pool_event.pool_location ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Message.to_conformist_error
  ;;

  let effects id =
    BaseGuard.ValidationSet.And
      [ Pool_location.Guard.Access.update id
      ; Pool_location.Guard.Access.File.create
      ]
  ;;
end

module DeleteFile : sig
  include Common.CommandSig with type t = Mapping.Id.t

  val decode : Conformist.input -> (t, Pool_message.Error.t) result
  val effects : Id.t -> Mapping.Id.t -> BaseGuard.ValidationSet.t
end = struct
  open Mapping

  type t = Id.t

  let command id = id
  let schema = Conformist.(make Field.[ Id.schema () ] command)

  let handle ?(tags = Logs.Tag.empty) (id : t) =
    Logs.info ~src (fun m -> m "Handle command DeleteFile" ~tags);
    Ok [ FileDeleted id |> Pool_event.pool_location ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Message.to_conformist_error
  ;;

  let effects location_id file_id =
    BaseGuard.ValidationSet.And
      [ Pool_location.Guard.Access.update location_id
      ; Pool_location.Guard.Access.File.delete file_id
      ]
  ;;
end
