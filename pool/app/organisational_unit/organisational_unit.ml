include Entity
include Event
module Guard = Entity_guard

let find = Repo.find
let all = Repo.all

module Repo = struct
  let sql_select_columns = Repo.sql_select_columns

  module Id = struct
    let t = Pool_common.Repo.Id.t
  end

  let t = Repo.t
end
