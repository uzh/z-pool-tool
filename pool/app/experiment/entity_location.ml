module Id = Pool_common.Id

module Room = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create room =
    if String.length room <= 0 then Error "Invalid room!" else Ok room
  ;;

  let to_human room = room
end

module Building = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create building =
    if String.length building <= 0
    then Error "Invalid building!"
    else Ok building
  ;;

  let to_human building = building
end

module Street = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create street =
    if String.length street <= 0 then Error "Invalid street!" else Ok street
  ;;

  let to_human street = street
end

module Zip = struct
  type t = string [@@deriving eq, show]

  let value t = t

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
end

module City = struct
  type t = string [@@deriving eq, show]

  let value t = t

  let create city =
    if String.length city <= 0 then Error "Invalid city!" else Ok city
  ;;

  let to_human city = city
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
