module Conformist = Pool_common.Utils.PoolConformist
module Utils = Pool_common.Utils
module Message = Pool_common.Message
module Field = Message.Field

module Mail = struct
  module Room = struct
    include Pool_common.Model.String

    let field = Field.Room
    let create = create
    let schema = schema ?validation:None field
    let of_string m = m
  end

  module Institution = struct
    include Pool_common.Model.String

    let field = Field.Institution
    let create = create
    let schema = schema ?validation:None field
    let of_string m = m
  end

  module Building = struct
    include Pool_common.Model.String

    let field = Field.Building
    let create = create
    let schema = schema ?validation:None field
    let of_string m = m
  end

  module Street = struct
    include Pool_common.Model.String

    let field = Field.Street
    let create = create
    let schema = schema ?validation:None field
    let of_string m = m
  end

  module Zip = struct
    include Pool_common.Model.String

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

    let schema = schema ~validation:create field
    let of_string m = m
  end

  module City = struct
    include Pool_common.Model.String

    let field = Field.City
    let create = create
    let schema = schema ?validation:None field
    let of_string m = m
  end

  type t =
    { institution : Institution.t option
    ; room : Room.t
    ; building : Building.t option
    ; street : Street.t
    ; zip : Zip.t
    ; city : City.t
    }
  [@@deriving eq, show]

  let create institution room building street zip city =
    let open CCResult in
    let* institution = institution |> CCResult.opt_map Institution.create in
    let* room = Room.create room in
    let* building = building |> CCResult.opt_map Building.create in
    let* street = Street.create street in
    let* zip = Zip.create zip in
    let* city = City.create city in
    Ok { institution; room; building; street; zip; city }
  ;;

  let command institution room building street zip city =
    { institution; room; building; street; zip; city }
  ;;

  let schema () =
    Conformist.(
      make
        Field.
          [ Conformist.optional @@ Institution.schema ()
          ; Room.schema ()
          ; Conformist.optional @@ Building.schema ()
          ; Street.schema ()
          ; Zip.schema ()
          ; City.schema ()
          ]
        command)
  ;;
end

type t =
  | Virtual
  | Physical of Mail.t
[@@deriving eq, show, variants]

let address_rows_human language address =
  match address with
  | Virtual ->
    ( Pool_common.Utils.field_to_string language Field.Virtual
      |> CCString.capitalize_ascii
    , ""
    , "" )
  | Physical address ->
    let open Mail in
    let building_room =
      [ address.building |> CCOption.map Building.value
      ; Some (Room.value address.room)
      ]
      |> CCList.filter_map CCFun.id
      |> CCString.concat " "
    in
    let zip_city =
      [ Zip.value address.zip; City.value address.city ] |> CCString.concat " "
    in
    building_room, Street.value address.street, zip_city
;;
