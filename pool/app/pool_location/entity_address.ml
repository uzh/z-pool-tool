module Conformist = Pool_common.Utils.PoolConformist
module Utils = Pool_common.Utils
module Message = Pool_common.Message
module Field = Message.Field

let create_if_not_empty_string field m =
  if CCString.is_empty m then Error Message.(Invalid field) else Ok m
;;

module Mail = struct
  module Room = struct
    type t = string [@@deriving eq, show]

    let field = Field.Room
    let create = create_if_not_empty_string field
    let value m = m
    let of_string m = m
    let schema () = Utils.schema_decoder create value field
  end

  module Building = struct
    type t = string [@@deriving eq, show]

    let field = Field.Building
    let create = create_if_not_empty_string field
    let value m = m
    let of_string m = m
    let schema () = Utils.schema_decoder create value field
  end

  module Street = struct
    type t = string [@@deriving eq, show]

    let field = Field.Street
    let create = create_if_not_empty_string field
    let value m = m
    let of_string m = m
    let schema () = Utils.schema_decoder create value field
  end

  module Zip = struct
    type t = string [@@deriving eq, show]

    let field = Field.Zip

    let create zip =
      let regex =
        Re.(
          seq [ repn (alt [ digit; set "_-" ]) 4 (Some 10) ]
          |> whole_string
          |> compile)
      in
      if Re.execp regex zip then Ok zip else Error Message.(Invalid field)
    ;;

    let value m = m
    let of_string m = m
    let schema () = Utils.schema_decoder create value field
  end

  module City = struct
    type t = string [@@deriving eq, show]

    let field = Field.City
    let create = create_if_not_empty_string field
    let value m = m
    let of_string m = m
    let schema () = Utils.schema_decoder create value field
  end

  type t =
    { room : Room.t
    ; building : Building.t option
    ; street : Street.t
    ; zip : Zip.t
    ; city : City.t
    }
  [@@deriving eq, show]

  let create room building street zip city =
    let open CCResult in
    let* room = Room.create room in
    let* building = building |> CCResult.opt_map Building.create in
    let* street = Street.create street in
    let* zip = Zip.create zip in
    let* city = City.create city in
    Ok { room; building; street; zip; city }
  ;;

  let command room building street zip city =
    { room; building; street; zip; city }
  ;;

  let schema () =
    Conformist.(
      make
        Field.
          [ Room.schema ()
          ; Conformist.optional @@ Building.schema ()
          ; Street.schema ()
          ; Zip.schema ()
          ; City.schema ()
          ]
        command)
  ;;
end

type t =
  | Address of Mail.t
  | Virtual
[@@deriving eq, show, variants]

let address_rows_human language address =
  match address with
  | Virtual ->
    ( Pool_common.Utils.field_to_string language Field.Virtual
      |> CCString.capitalize_ascii
    , ""
    , "" )
  | Address address ->
    let open Mail in
    let building_room =
      match address.building with
      | None -> Room.value address.room
      | Some building ->
        [ Building.value building; Room.value address.room ]
        |> CCString.concat " "
    in
    let zip_city =
      [ Zip.value address.zip; City.value address.city ] |> CCString.concat " "
    in
    building_room, Street.value address.street, zip_city
;;
