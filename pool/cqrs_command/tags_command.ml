module Conformist = Pool_conformist
module Message = Pool_message

let src = Logs.Src.create "tags.cqrs"

type decoded =
  { title : Tags.Title.t
  ; description : Tags.Description.t option
  ; model : Tags.Model.t
  }

let default_command title description model = { title; description; model }

let assignment_schema =
  let open Conformist in
  make Field.[ Tags.Id.schema ~field:Pool_message.Field.Tag () ] CCFun.id
;;

let default_schema =
  Conformist.(
    make
      Field.
        [ Tags.Title.schema ()
        ; Conformist.optional @@ Tags.Description.schema ()
        ; Tags.Model.schema ()
        ]
      default_command)
;;

let validate expected { Tags.id; model; _ } =
  if Tags.Model.(equal expected model)
  then Ok id
  else Error Message.(Error.Invalid Field.Tag)
;;

module Create : sig
  include Common.CommandSig with type t = decoded

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result

  val handle
    :  ?id:Tags.Id.t
    -> ?tags:Logs.Tag.set
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result
end = struct
  type t = decoded

  let handle ?id ?(tags = Logs.Tag.empty) ({ title; description; model } : t) =
    let open CCResult in
    Logs.info ~src (fun m -> m "Handle command Create" ~tags);
    Tags.create ?id ?description title model
    >|= fun tag -> [ Tags.Created tag |> Pool_event.tags ]
  ;;

  let decode data =
    Conformist.decode_and_validate default_schema data
    |> CCResult.map_err Message.to_conformist_error
  ;;

  let effects = Tags.Guard.Access.create
end

module Update : sig
  include Common.CommandSig with type t = decoded

  val decode : (string * string list) list -> (t, Pool_message.Error.t) result

  val handle
    :  ?tags:Logs.Tag.set
    -> Tags.t
    -> t
    -> (Pool_event.t list, Pool_message.Error.t) result

  val effects : Tags.Id.t -> Guard.ValidationSet.t
end = struct
  type t = decoded

  let handle ?(tags = Logs.Tag.empty) tag (command : t) =
    let open Tags in
    if Model.equal tag.Tags.model command.model
    then (
      let tag : t =
        { tag with title = command.title; description = command.description }
      in
      Logs.info ~src (fun m -> m "Handle command edit" ~tags);
      Ok [ Updated tag |> Pool_event.tags ])
    else Error Pool_message.(Error.Invalid Field.Model)
  ;;

  let decode data =
    Conformist.decode_and_validate default_schema data
    |> CCResult.map_err Message.to_conformist_error
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

  val validate : Tags.t -> (t, Conformist.error_msg) result
  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Tags.Id.t

  let handle ?(tags = Logs.Tag.empty) contact (tag_uuid : t) =
    Logs.info ~src (fun m -> m "Handle command AssignTagToContact" ~tags);
    Ok
      [ Tags.(Tagged { Tagged.model_uuid = Contact.id contact; tag_uuid })
        |> Pool_event.tags
      ]
  ;;

  let validate = validate Tags.Model.Contact

  let decode data =
    Conformist.decode_and_validate assignment_schema data
    |> CCResult.map_err Message.to_conformist_error
  ;;

  let effects = Tags.Guard.Access.assign Contact.Guard.Access.update
end

module RemoveTagFromContact : sig
  include Common.CommandSig with type t = Tags.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Contact.t
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val effects : Contact.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Tags.t

  let handle ?(tags = Logs.Tag.empty) contact tag =
    Logs.info ~src (fun m -> m "Handle command RemoveTagFromContact" ~tags);
    let open CCResult in
    let* tag_uuid = validate Tags.Model.Contact tag in
    let model_uuid = Contact.id contact in
    Ok [ Tags.(Untagged { Tagged.model_uuid; tag_uuid }) |> Pool_event.tags ]
  ;;

  let effects = Tags.Guard.Access.remove Contact.Guard.Access.update
end

module AssignTagToExperiment : sig
  include Common.CommandSig with type t = Tags.Id.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Experiment.t
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val validate : Tags.t -> (t, Conformist.error_msg) result
  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Tags.Id.t

  let handle ?(tags = Logs.Tag.empty) { Experiment.id; _ } (tag_uuid : t) =
    Logs.info ~src (fun m -> m "Handle command AssignTagToExperiment" ~tags);
    let model_uuid = Experiment.Id.to_common id in
    Ok [ Tags.(Tagged { Tagged.model_uuid; tag_uuid }) |> Pool_event.tags ]
  ;;

  let validate = validate Tags.Model.Experiment

  let decode data =
    Conformist.decode_and_validate assignment_schema data
    |> CCResult.map_err Message.to_conformist_error
  ;;

  let effects = Tags.Guard.Access.assign Experiment.Guard.Access.update
end

module RemoveTagFromExperiment : sig
  include Common.CommandSig with type t = Tags.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Experiment.t
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Tags.t

  let handle ?(tags = Logs.Tag.empty) { Experiment.id; _ } tag =
    Logs.info ~src (fun m -> m "Handle command RemoveTagFromExperiment" ~tags);
    let open CCResult in
    let* tag_uuid = validate Tags.Model.Experiment tag in
    let model_uuid = Experiment.Id.to_common id in
    Ok [ Tags.(Untagged { Tagged.model_uuid; tag_uuid }) |> Pool_event.tags ]
  ;;

  let effects = Tags.Guard.Access.remove Experiment.Guard.Access.update
end

module AssignParticipationTagToEntity : sig
  include Common.CommandSig with type t = Tags.Id.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Tags.ParticipationTags.entity
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val validate : Tags.t -> (t, Conformist.error_msg) result
  val decode : (string * string list) list -> (t, Pool_message.Error.t) result
  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Tags.Id.t

  let handle ?(tags = Logs.Tag.empty) entity (tag_uuid : t) =
    Logs.info ~src (fun m ->
      m "Handle command AssignParticipationTagToEntity" ~tags);
    Ok Tags.[ ParticipationTagAssigned (entity, tag_uuid) |> Pool_event.tags ]
  ;;

  let validate = validate Tags.Model.Experiment

  let decode data =
    Conformist.decode_and_validate assignment_schema data
    |> CCResult.map_err Message.to_conformist_error
  ;;

  let effects id = Experiment.Guard.Access.update id
end

module RemoveParticipationTagFromEntity : sig
  include Common.CommandSig with type t = Tags.t

  val handle
    :  ?tags:Logs.Tag.set
    -> Tags.ParticipationTags.entity
    -> t
    -> (Pool_event.t list, Conformist.error_msg) result

  val effects : Experiment.Id.t -> Guard.ValidationSet.t
end = struct
  type t = Tags.t

  let handle ?(tags = Logs.Tag.empty) entity tag =
    Logs.info ~src (fun m ->
      m "Handle command RemoveParticipationTagFromEntity" ~tags);
    Ok Tags.[ ParticipationTagRemoved (entity, tag.Tags.id) |> Pool_event.tags ]
  ;;

  let effects id = Experiment.Guard.Access.update id
end
