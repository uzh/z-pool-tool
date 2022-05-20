module Conformist = Pool_common.Utils.PoolConformist
module Message = Pool_common.Message

module Create = struct
  type file_base =
    { label : Pool_location.Mapping.Label.t
    ; language : Pool_common.Language.t
    ; asset_id : Pool_common.Id.t
    }
  [@@deriving eq, show, sexp]

  type base =
    { name : Pool_location.Name.t
    ; description : Pool_location.Description.t option
    ; link : Pool_location.Link.t option
    ; files : file_base list
    }

  type address = Pool_location.Address.t

  type t =
    { name : Pool_location.Name.t
    ; description : Pool_location.Description.t option
    ; link : Pool_location.Link.t option
    ; address : address
    ; files : file_base list
    }

  let command_base name description link files =
    { name; description; link; files }
  ;;

  let schema_mail_address = Pool_location.Address.Mail.schema ()

  let schema_files ()
      : (Conformist.error_msg, file_base list) Conformist.Field.t
    =
    let open Sexplib in
    Pool_common.Utils.schema_list_decoder
      (fun m ->
        m
        |> CCList.map (fun k -> k |> Sexp.of_string |> file_base_of_sexp)
        |> CCResult.pure)
      (fun m ->
        m |> CCList.map (fun k -> k |> sexp_of_file_base |> Sexp.to_string))
      Pool_common.Message.Field.FileMapping
  ;;

  let schema =
    Conformist.(
      make
        Field.
          [ Pool_location.Name.schema ()
          ; Conformist.optional @@ Pool_location.Description.schema ()
          ; Conformist.optional @@ Pool_location.Link.schema ()
          ; schema_files ()
          ]
        command_base)
  ;;

  let handle
      ?(id = Pool_location.Id.create ())
      { name; description; link; address; files }
    =
    let open CCResult in
    let* location =
      Pool_location.create
        ~id
        name
        description
        address
        link
        Pool_location.Status.init
        []
    in
    let files =
      CCList.map
        (fun { label; language; asset_id } ->
          Pool_location.Mapping.Write.create
            label
            language
            asset_id
            (id |> Pool_location.Id.value |> Pool_common.Id.of_string))
        files
    in
    Ok [ Pool_location.Created (location, files) |> Pool_event.pool_location ]
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
         | true -> Ok Pool_location.Address.Virtual
         | false ->
           Conformist.decode_and_validate schema_mail_address data
           >|= Pool_location.Address.address
       in
       Ok
         { name = base.name
         ; description = base.description
         ; link = base.link
         ; address
         ; files = base.files
         }
  ;;

  let can user _ =
    Permission.can
      user
      ~any_of:[ Permission.Manage (Permission.Location, None) ]
  ;;
end

module Update = struct
  type base =
    { name : Pool_location.Name.t
    ; description : Pool_location.Description.t option
    ; link : Pool_location.Link.t option
    ; status : Pool_location.Status.t
    }

  type address = Pool_location.Address.t

  type t =
    { name : Pool_location.Name.t
    ; description : Pool_location.Description.t option
    ; address : address
    ; link : Pool_location.Link.t option
    ; status : Pool_location.Status.t
    }

  let command_base name description link status =
    { name; description; link; status }
  ;;

  let schema_mail_address = Pool_location.Address.Mail.schema ()

  let schema =
    Conformist.(
      make
        Field.
          [ Pool_location.Name.schema ()
          ; Conformist.optional @@ Pool_location.Description.schema ()
          ; Conformist.optional @@ Pool_location.Link.schema ()
          ; Pool_location.Status.schema ()
          ]
        command_base)
  ;;

  let handle location update =
    Ok [ Pool_location.Updated (location, update) |> Pool_event.pool_location ]
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
         | true -> Ok Pool_location.Address.Virtual
         | false ->
           Conformist.decode_and_validate schema_mail_address data
           >|= Pool_location.Address.address
       in
       Ok
         { name = base.name
         ; description = base.description
         ; address
         ; link = base.link
         ; status = base.status
         }
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end
