module Conformist = Pool_common.Utils.PoolConformist
module Message = Pool_common.Message
open Pool_location

let src = Logs.Src.create "location.cqrs"

module Create = struct
  type base =
    { name : Name.t
    ; description : Description.t option
    ; link : Link.t option
    }

  type address = Address.t

  type t =
    { name : Name.t
    ; description : Description.t option
    ; link : Link.t option
    ; address : address
    }

  let command_base name description link = { name; description; link }
  let schema_mail_address = Address.Mail.schema ()

  let schema =
    Conformist.(
      make
        Field.
          [ Name.schema ()
          ; Conformist.optional @@ Description.schema ()
          ; Conformist.optional @@ Link.schema ()
          ]
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

  let decode data =
    let open CCResult in
    map_err Message.to_conformist_error
    @@ let* base = Conformist.decode_and_validate schema data in
       let* address =
         match
           CCList.assoc ~eq:( = ) Message.Field.(Virtual |> show) data
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
         ; description = base.description
         ; link = base.link
         ; address
         }
  ;;

  let can = [ `Create, `Entity `Location ]
end

module Update = struct
  type base =
    { name : Name.t
    ; description : Description.t option
    ; link : Link.t option
    ; status : Status.t
    }

  type address = Address.t

  let command_base name description link status =
    { name; description; link; status }
  ;;

  let schema_mail_address = Address.Mail.schema ()

  let schema =
    Conformist.(
      make
        Field.
          [ Name.schema ()
          ; Conformist.optional @@ Description.schema ()
          ; Conformist.optional @@ Link.schema ()
          ; Status.schema ()
          ]
        command_base)
  ;;

  let handle ?(tags = Logs.Tag.empty) location update =
    Logs.info ~src (fun m -> m "Handle command Update" ~tags);
    Ok [ Updated (location, update) |> Pool_event.pool_location ]
  ;;

  let decode data =
    let open CCResult in
    map_err Message.to_conformist_error
    @@ let* base = Conformist.decode_and_validate schema data in
       let* address_new =
         match
           CCList.assoc ~eq:( = ) Message.Field.(Virtual |> show) data
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
         ; description = base.description
         ; address = address_new
         ; link = base.link
         ; status = base.status
         }
  ;;

  let can t =
    [ `Update, `Entity `Location
    ; `Update, `Target (t.id |> Guard.Uuid.target_of Id.value)
    ]
  ;;
end

module AddFile = struct
  open Mapping

  type t = file_base

  let command label language asset_id = { label; language; asset_id }

  let schema =
    let fcn_ok = Utils.fcn_ok in
    Conformist.(
      make
        Field.
          [ Label.schema ()
          ; Pool_common.Language.schema ()
          ; Pool_common.(
              Utils.schema_decoder
                (fcn_ok Id.of_string)
                Id.value
                Message.Field.FileMapping)
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
        asset_id
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

  let effects = [ `Manage, `Entity `Location ]
end

module DeleteFile = struct
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

  let effects = [ `Manage, `Entity `Location ]
end
