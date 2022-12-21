open Entity

type event = DefaultRestored of t list [@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | DefaultRestored templates ->
    (* TODO: Delete all old templates *)
    Lwt_list.iter_s (Repo.insert pool) templates
;;
