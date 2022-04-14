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
let find_by_url_prefix = Repo.find_by_url_prefix Database.root
let find_all = Repo.find_all Database.root
let find_databases = Repo.find_databases Database.root

let find_styles db_pool =
  let open Utils.Lwt_result.Infix in
  Repo.find_by_label Database.root db_pool >|= fun { styles; _ } -> styles
;;

(* Logo mappings *)
module LogoMapping = struct
  include LogoMapping
end
