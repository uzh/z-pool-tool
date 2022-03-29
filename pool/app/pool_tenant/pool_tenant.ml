include Entity
include Event

module Url = struct
  include Entity.Url

  let of_pool = Repo.Url.of_pool
end

let to_ctx pool = [ "pool", Database.Label.value pool ]
let find = Repo.find Database.root
let find_full = Repo.find_full Database.root
let find_by_label = Repo.find_by_label Database.root
let find_all = Repo.find_all Database.root
let find_databases = Repo.find_databases Database.root

let find_styles db_pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_by_label Database.root db_pool >|= fun { styles; _ } -> styles
;;

type handle_list_recruiters = unit -> Sihl_user.t list Lwt.t
type handle_list_tenants = unit -> t list Lwt.t

module Selection = struct
  include Selection

  let find_prefixed = Repo.find_selectable Database.root
end

(* Logo mappings *)
module LogoMapping = struct
  include LogoMapping
end
