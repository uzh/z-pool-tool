module Id = struct
  type t = string [@@deriving eq, show]

  let to_human id = id
  let t = Caqti_type.string
end

module Title = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module Description = struct
  type t = string [@@deriving eq, show]

  let t = Caqti_type.string
end

module ExperimentDate = struct
  type t = Ptime.t [@@deriving eq, show]

  let t = Caqti_type.ptime
end

module Location = struct
  type t =
    { id : string
    ; room : string
    ; building : string
    ; street : string
    ; zip : string
    ; city : string
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
           string
           (tup2 string (tup2 string (tup2 string (tup2 string string))))))
  ;;
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t
  ; experiment_date : ExperimentDate.t
  ; location : Location.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]
