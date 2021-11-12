module Key = struct
  type t = string
end

module Content = struct
  type t = string
end

module Property = struct
  type t =
    { language : Pool_common.Language.t
    ; key : Key.t
    ; content : Content.t
    }
end
