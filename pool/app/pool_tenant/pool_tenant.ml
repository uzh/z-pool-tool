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

let public_scheme () =
  let is_localhost =
    Uri.of_string
    %> Uri.host
    %> CCOption.exists (CCString.lowercase_ascii %> CCString.equal "localhost")
  in
  match
    ( Pool_core.Configuration.is_production ()
    , Pool_core.Configuration.read_string "PUBLIC_URL" )
  with
  | true, _ -> "https"
  | false, Some url when is_localhost url -> "http"
  | false, _ -> "https"
;;

let create_public_url pool_url =
  Webserver.externalize_path
  %> Format.asprintf "%s://%s%s" (public_scheme ()) (Url.value pool_url)
;;

let clear_cache = Repo.Cache.clear

module Repo = struct
  module Id = Repo_entity.Id
end
