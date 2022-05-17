module Mapping = Entity_file_mapping

module Id = struct
  include Pool_common.Id
end

module Name = struct
  type t = string [@@deriving eq, show]

  let create m = m
  let value m = m
end

module Description = struct
  type t = string [@@deriving eq, show]

  let create m = m
  let value m = m
end

module MailingAddress = struct
  module Room = struct
    type t = string [@@deriving eq, show]

    let value m = m

    let create room =
      if CCString.is_empty room then Error "Invalid room!" else Ok room
    ;;
  end

  module Building = struct
    type t = string [@@deriving eq, show]

    let value m = m

    let create building =
      if CCString.is_empty building
      then Error "Invalid building!"
      else Ok building
    ;;
  end

  module Street = struct
    type t = string [@@deriving eq, show]

    let value m = m

    let create street =
      if CCString.is_empty street then Error "Invalid street!" else Ok street
    ;;
  end

  module Zip = struct
    type t = string [@@deriving eq, show]

    let value m = m

    let create zip =
      let regex =
        Re.(
          seq [ repn (alt [ digit; set "_-" ]) 4 (Some 10) ]
          |> whole_string
          |> compile)
      in
      if Re.execp regex zip then Ok zip else Error "Invalid zip code!"
    ;;
  end

  module City = struct
    type t = string [@@deriving eq, show]

    let value m = m

    let create city =
      if CCString.is_empty city then Error "Invalid city!" else Ok city
    ;;
  end

  type t =
    { room : Room.t
    ; building : Building.t
    ; street : Street.t
    ; zip : Zip.t
    ; city : City.t
    }
  [@@deriving eq, show]

  let create room building street zip city =
    let open CCResult in
    let* room = Room.create room in
    let* building = Building.create building in
    let* street = Street.create street in
    let* zip = Zip.create zip in
    let* city = City.create city in
    Ok { room; building; street; zip; city }
  ;;

  let value m = m
end

module Link = struct
  type t = string [@@deriving eq, show]

  let create m = m
  let value m = m
end

module Status = struct
  let printing m fmt _ = Format.pp_print_string fmt m

  type t =
    | Active [@name "active"] [@printer printing "active"]
    | Maintenance [@name "maintenance"] [@printer printing "maintenance"]
    | Closed [@name "closed"] [@printer printing "closed"]
  [@@deriving eq, show { with_path = false }, yojson, sexp_of]

  let read m =
    m |> Format.asprintf "[\"%s\"]" |> Yojson.Safe.from_string |> t_of_yojson
  ;;

  let init = Active
end

type t =
  { id : Id.t
  ; name : Name.t
  ; description : Description.t option
  ; address : MailingAddress.t option
  ; link : Link.t option
  ; status : Status.t
  ; files : Mapping.file list
  ; created_at : Pool_common.CreatedAt.t
  ; updated_at : Pool_common.UpdatedAt.t
  }
[@@deriving eq, show]

let create ?(id = Id.create ()) name description address link status files =
  { id
  ; name
  ; description
  ; address
  ; link
  ; status
  ; files
  ; created_at = Pool_common.CreatedAt.create ()
  ; updated_at = Pool_common.UpdatedAt.create ()
  }
;;
