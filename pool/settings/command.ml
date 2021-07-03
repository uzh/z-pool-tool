module Change = struct
  type t = { new_settings : (string * string) list }
  type handle = t -> (Event.t list, string) Result.t
end
