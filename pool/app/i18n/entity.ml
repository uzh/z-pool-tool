module Key = struct
  type t = string
end

module Content = struct
  type t = string
end

module Property = struct
  type t =
    { language : Settings.Language.t
    ; key : Key.t
    ; content : Content.t
    }
end
