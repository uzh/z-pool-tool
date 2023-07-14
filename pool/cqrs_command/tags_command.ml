module Conformist = Pool_common.Utils.PoolConformist

let src = Logs.Src.create "tags.cqrs"

module Create : sig
  type create =
    { title : Tags.Title.t
    ; description : Tags.Description.t option
    }

  include Common.CommandSig with type t = create

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val handle
    :  ?id:Tags.Id.t
    -> ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_common.Message.error) result
end = struct
  type create =
    { title : Tags.Title.t
    ; description : Tags.Description.t option
    }

  type t = create

  let command title description = { title; description }

  let schema =
    Conformist.(
      make
        Field.
          [ Tags.Title.schema ()
          ; Conformist.optional @@ Tags.Description.schema ()
          ]
        command)
  ;;

  let handle ?id ?(tags = Logs.Tag.empty) ({ title; description } : t) =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    Tags.create ?id ?description title
    >|= fun tag -> [ Tags.Created tag |> Pool_event.tags ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Tags.Guard.Access.create
end

module Update : sig
  include Common.CommandSig with type t = Tags.t

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Tags.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Tags.t

  let command id title description = { Tags.id; title; description }

  let schema =
    Conformist.(
      make
        Field.
          [ Tags.Id.schema ()
          ; Tags.Title.schema ()
          ; Conformist.optional @@ Tags.Description.schema ()
          ]
        command)
  ;;

  let handle ?(tags = Logs.Tag.empty) (command : t) =
    Logs.info ~src (fun m -> m "Handle command edit" ~tags);
    Ok [ Tags.Updated command |> Pool_event.tags ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Tags.Guard.Access.update
end

module AssignTagToContact : sig
  include Common.CommandSig with type t = Tags.Id.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Contact.t
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Tags.Id.t

  let schema =
    Conformist.(
      make
        Field.[ Tags.Id.schema ~field:Pool_common.Message.Field.Tag () ]
        CCFun.id)
  ;;

  let handle ?(tags = Logs.Tag.empty) contact (tag_uuid : t) =
    Logs.info ~src (fun m -> m "Handle command AssignTagToContact" ~tags);
    Ok
      [ Tags.Tagged
          Tags.
            { Tagged.model = Model.Contact
            ; model_uuid = Contact.id contact
            ; tag_uuid
            }
        |> Pool_event.tags
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Tags.Guard.Access.assign Contact.Guard.Access.read
end

module RemoveTagFromContact : sig
  include Common.CommandSig with type t = Tags.Id.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Contact.t
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val decode
    :  (string * string list) list
    -> (t, Pool_common.Message.error) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Tags.Id.t

  let schema =
    Conformist.(
      make
        Field.[ Tags.Id.schema ~field:Pool_common.Message.Field.Tag () ]
        CCFun.id)
  ;;

  let handle ?(tags = Logs.Tag.empty) contact (tag_uuid : t) =
    let open Tags in
    Logs.info ~src (fun m -> m "Handle command RemoveTagFromContact" ~tags);
    Ok
      [ Untagged
          { Tagged.model = Model.Contact
          ; model_uuid = Contact.id contact
          ; tag_uuid
          }
        |> Pool_event.tags
      ]
  ;;

  let decode data =
    Conformist.decode_and_validate schema data
    |> CCResult.map_err Pool_common.Message.to_conformist_error
  ;;

  let effects = Tags.Guard.Access.remove Contact.Guard.Access.read
end
