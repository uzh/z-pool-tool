module Id = struct
  type t = string
end

module Title = struct
  type t = string
end

module Relation = struct
  type t =
    | Participant
    | Experiment
    | Pool
end

module Description = struct
  type t = string
end

type 'a tag =
  { id : Id.t
  ; title : Title.t
  ; description : Description.t
  ; relation : Relation.t
  ; owner : 'a Admin.t
  ; created_at : Ptime.t
  ; updated_at : Ptime.t
  }
[@@deriving eq, show]
