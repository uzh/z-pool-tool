module Id = struct
  type t = string [@@deriving eq, show]

  let create () = Uuidm.create `V4 |> Uuidm.to_string
  let to_human id = id
  let t = Caqti_type.string
end

module Room = struct
  type t = string [@@deriving eq, show]

  let create room =
    if String.length room <= 0 then Error "Invalid room!" else Ok room
  ;;

  let to_human room = room
  let t = Caqti_type.string
end

module Building = struct
  type t = string [@@deriving eq, show]

  let create building =
    if String.length building <= 0
    then Error "Invalid building!"
    else Ok building
  ;;

  let to_human building = building
  let t = Caqti_type.string
end

module Street = struct
  type t = string [@@deriving eq, show]

  let create street =
    if String.length street <= 0 then Error "Invalid street!" else Ok street
  ;;

  let to_human street = street
  let t = Caqti_type.string
end

module Zip = struct
  type t = string [@@deriving eq, show]

  let create zip =
    let regex =
      Re.(
        seq [ repn (alt [ digit; set "_-" ]) 4 (Some 10) ]
        |> whole_string
        |> compile)
    in
    if Re.execp regex zip then Ok zip else Error "Invalid zip code!"
  ;;

  let to_human zip = zip
  let t = Caqti_type.string
end

module City = struct
  type t = string [@@deriving eq, show]

  let create city =
    if String.length city <= 0 then Error "Invalid city!" else Ok city
  ;;

  let to_human city = city
  let t = Caqti_type.string
end

type t =
  { id : Id.t
  ; room : Room.t
  ; building : Building.t
  ; street : Street.t
  ; zip : Zip.t
  ; city : City.t
  }
[@@deriving eq, show]

let encode m = Ok (m.id, (m.room, (m.building, (m.street, (m.zip, m.city)))))

let decode (id, (room, (building, (street, (zip, city))))) =
  Ok { id; room; building; street; zip; city }
;;

let t =
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Id.t
         (tup2 Room.t (tup2 Building.t (tup2 Street.t (tup2 Zip.t City.t))))))
;;
