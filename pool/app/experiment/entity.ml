module Id = Pool_common.Id
module Common = Pool_common

module Title = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create title =
    if CCString.is_empty title
    then Error Common.Message.(Invalid Field.Title)
    else Ok title
  ;;

  let schema () = Common.(Utils.schema_decoder create value Message.Field.Title)
end

module Description = struct
  type t = string [@@deriving eq, show]

  let value m = m

  let create description =
    if CCString.is_empty description
    then Error Pool_common.Message.(Invalid Field.Description)
    else Ok description
  ;;

  let schema () =
    Common.(Utils.schema_decoder create value Message.Field.Description)
  ;;
end

type t =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t
  ; filter : string
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]

let create ?id title description =
  { id = id |> CCOption.value ~default:(Id.create ())
  ; title
  ; description
  ; filter = "1=1"
  ; created_at = Ptime_clock.now ()
  ; updated_at = Ptime_clock.now ()
  }
;;

let title_value (m : t) = Title.value m.title
let description_value (m : t) = Description.value m.description

module Public = struct
  type t =
    { id : Pool_common.Id.t
    ; description : Description.t
    }
  [@@deriving eq, show]
end
