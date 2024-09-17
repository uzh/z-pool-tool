include Entity
include Event
module Guard = Entity_guard
module VersionHistory = Changelog.T (Entity)

let find = Repo.find
let all = Repo.all
let find_by = Repo.find_by

module Repo = struct
  let sql_select_columns = Repo.sql_select_columns

  module Id = struct
    let t = Pool_common.Repo.Id.t
  end

  let t = Repo.t
end
