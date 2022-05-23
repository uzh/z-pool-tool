module Conformist = Pool_common.Utils.PoolConformist
module Message = Pool_common.Message
open Pool_location

module Create = struct
  type file_base =
    { label : Mapping.Label.t
    ; language : Pool_common.Language.t
    ; asset_id : Pool_common.Id.t
    }
  [@@deriving eq, show, sexp]

  type base =
    { name : Name.t
    ; description : Description.t option
    ; link : Link.t option
    ; files : file_base list
    }

  type address = Address.t

  type t =
    { name : Name.t
    ; description : Description.t option
    ; link : Link.t option
    ; address : address
    ; files : file_base list
    }

  let command_base name description link files =
    { name; description; link; files }
  ;;

  let schema_mail_address = Address.Mail.schema ()

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
          [ Name.schema ()
          ; Conformist.optional @@ Description.schema ()
          ; Conformist.optional @@ Link.schema ()
          ; schema_files ()
          ]
        command_base)
  ;;

  let handle
      ?(id = Id.create ())
      ({ name; description; link; address; files } : t)
    =
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
    let files =
      CCList.map
        (fun { label; language; asset_id } ->
          Mapping.Write.create
            label
            language
            asset_id
            (id |> Id.value |> Pool_common.Id.of_string))
        files
    in
    Ok [ Created (location, files) |> Pool_event.pool_location ]
  ;;

  let decode data =
    let open CCResult in
    print_endline
      (CCString.concat
         ", "
         (CCList.map
            (fun (key, value) ->
              CCString.concat ", " value |> Format.asprintf "%s: %s" key)
            data));
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
           >|= Address.address
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

  let handle location update =
    Ok [ Updated (location, update) |> Pool_event.pool_location ]
  ;;

  let decode data =
    let open CCResult in
    let to_bool m =
      m
      |> CCList.inter ~eq:CCString.equal [ "on"; "checked"; "true" ]
      |> CCList.is_empty
      |> not
    in
    print_endline
      (CCString.concat
         ", "
         (CCList.map
            (fun (key, value) ->
              CCString.concat ", " value |> Format.asprintf "%s: %s" key)
            data));
    map_err Message.to_conformist_error
    @@ let* base = Conformist.decode_and_validate schema data in
       let* address_new =
         match
           CCList.assoc_opt ~eq:( = ) Message.Field.(Virtual |> show) data
           |> CCOption.map_or ~default:false to_bool
         with
         | true -> Ok Address.Virtual
         | false ->
           Conformist.decode_and_validate schema_mail_address data
           >|= Address.address
       in
       Ok
         { name = base.name
         ; description = base.description
         ; address = address_new
         ; link = base.link
         ; status = base.status
         }
  ;;

  let can user _ =
    Permission.can user ~any_of:[ Permission.Manage (Permission.System, None) ]
  ;;
end
