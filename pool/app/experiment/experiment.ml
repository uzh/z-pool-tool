include Entity
include Event

type add = t -> t Lwt.t
type update = t -> t Lwt.t
type destroy = t -> t Lwt.t
