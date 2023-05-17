include Entity
include Event
module Service = Pool_tenant_service

module SmtpAuth = struct
  include Entity.SmtpAuth
  include Repo.Smtp
end

module Url = struct
  include Entity.Url

  let of_pool = Repo.Url.of_pool
end

let find = Repo.find Pool_database.root
let find_full = Repo.find_full Pool_database.root
let find_by_label = Repo.find_by_label Pool_database.root
let find_all = Repo.find_all Pool_database.root
let find_databases = Repo.find_databases Pool_database.root

type handle_list_recruiters = unit -> Sihl_user.t list Lwt.t
type handle_list_tenants = unit -> t list Lwt.t

module Selection = struct
  include Selection

  let find_all = Repo.find_selectable Pool_database.root
end

module LogoMapping = struct
  include LogoMapping
end

module Guard = struct
  include Entity_guard
end
