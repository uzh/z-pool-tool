module OneOf2 = struct
  type ('a, 'b) t =
    | One of 'a
    | Two of 'b
  [@@deriving variants, eq, show]
end

module OneOf3 = struct
  type ('a, 'b, 'c) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
  [@@deriving variants, eq, show]
end

module OneOf4 = struct
  type ('a, 'b, 'c, 'd) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
    | Four of 'd
  [@@deriving variants, eq, show]
end

module OneOf5 = struct
  type ('a, 'b, 'c, 'd, 'e) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
    | Four of 'd
    | Five of 'e
  [@@deriving variants, eq, show]
end

module OneOf6 = struct
  type ('a, 'b, 'c, 'd, 'e, 'f) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
    | Four of 'd
    | Five of 'e
    | Six of 'f
  [@@deriving variants, eq, show]
end

module OneOf7 = struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
    | Four of 'd
    | Five of 'e
    | Six of 'f
    | Seven of 'g
  [@@deriving variants, eq, show]
end

module OneOf8 = struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t =
    | One of 'a
    | Two of 'b
    | Three of 'c
    | Four of 'd
    | Five of 'e
    | Six of 'f
    | Seven of 'g
    | Eight of 'h
  [@@deriving variants, eq, show]
end
