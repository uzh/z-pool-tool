open CCFun.Infix
include Entity
include Event
module LogoMapping = LogoMapping
module Guard = Entity_guard

type handle_list_recruiters = unit -> Pool_user.t list Lwt.t
type handle_list_tenants = unit -> t list Lwt.t

module Url = struct
  include Entity.Url

  let of_pool = Repo_entity.Url.of_pool
end

let find = Repo.find Database.root
let find_full = Repo.find_full Database.root
let find_by_label = Repo.find_by_label Database.root
let find_by_url = Repo.find_by_url Database.root
let find_all = Repo.find_all Database.root

let find_gtx_api_key_and_url_by_label =
  Repo.find_gtx_api_key_and_url_by_label Database.root
;;

let create_public_url pool_url =
  Sihl.Web.externalize_path
  %> Format.asprintf "https://%s%s" (Url.value pool_url)
;;

let clear_cache = Repo.Cache.clear
