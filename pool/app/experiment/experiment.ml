include Entity
include Event
module Filter = Utils.Filter

type handle_list_experiments = Filter.Ql.t -> t list Lwt.t
type add = t -> t Lwt.t
type update = t -> t Lwt.t
type destroy = t -> t Lwt.t
