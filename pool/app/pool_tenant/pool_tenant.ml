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

let find = Repo.find Database.Pool.Root.label
let find_full = Repo.find_full Database.Pool.Root.label
let find_by_label = Repo.find_by_label Database.Pool.Root.label
let find_by_url ?should_cache = Repo.find_by_url ?should_cache Database.Pool.Root.label
let find_all = Repo.find_all Database.Pool.Root.label

let find_gtx_api_key_and_url_by_label =
  Repo.find_gtx_api_key_and_url_by_label Database.Pool.Root.label
;;

let create_public_url pool_url =
  Sihl.Web.externalize_path %> Format.asprintf "https://%s%s" (Url.value pool_url)
;;

let clear_cache = Repo.Cache.clear

module Repo = struct
  module Id = Repo_entity.Id
end
