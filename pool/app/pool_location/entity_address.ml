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

  module Institution = struct
    type t = string [@@deriving eq, show]

    let field = Field.Institution
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

  let to_html mail =
    let open Tyxml.Html in
    let { institution; room; building; street; zip; city } = mail in
    let building_room =
      match building with
      | Some building -> Format.asprintf "%s %s" room building
      | None -> room
    in
    let city_zip = Format.asprintf "%s %s" city zip in
    let base = [ building_room; street; city_zip ] in
    (match institution with
    | Some institution -> CCList.cons institution base
    | None -> base)
    |> CCList.foldi
         (fun html index str ->
           let str = str |> txt in
           if index = 0 then [ strong [ str ] ] else html @ [ br (); str ])
         []
    |> span
  ;;
end

type t =
  | Virtual
  | Physical of Mail.t
[@@deriving eq, show, variants]

let to_html language =
  let open Tyxml.Html in
  function
  | Virtual ->
    strong
      [ txt
          (Pool_common.(Utils.field_to_string language Field.Virtual)
          |> CCString.capitalize_ascii)
      ]
  | Physical mail -> Mail.to_html mail
;;

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
