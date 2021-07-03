module Add_tenant = struct
  type t =
    { title : string
    ; description : string
    ; url : string
    ; database : string
    ; styles : string
    ; icon : string
    ; logos : string
    ; partner_logos : string
    }

  type handle = t -> (Event.t list, string) Result.t
  type can = Sihl.User.t -> bool Lwt.t
end

module Edit_tenant = struct
  type t =
    { tenant_id : string
    ; title : string
    ; description : string
    ; url : string
    ; database : string
    ; styles : string
    ; icon : string
    ; logos : string
    ; partner_logos : string
    }

  type handle = t -> Entity.tenant -> (Event.t list, string) Result.t
  type can = Sihl.User.t -> bool Lwt.t
end

module Add_operator = struct
  type t =
    { operator_id : string
    ; tenant_id : string
    }

  type handle =
    t -> Sihl.User.t -> Entity.tenant -> (Event.t list, string) Result.t

  type can = Sihl.User.t -> bool Lwt.t
end

module Activate_operator = struct
  type t = { operator_id : string }
  type handle = t -> Sihl.User.t -> (Event.t list, string) Result.t
  type can = Sihl.User.t -> bool Lwt.t
end

module Deactivate_operator = struct
  type t = { operator_id : string }
  type handle = t -> Sihl.User.t -> (Event.t list, string) Result.t
  type can = Sihl.User.t -> bool Lwt.t
end
