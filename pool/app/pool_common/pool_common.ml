include Entity
module Repo = Repo
module Error = Pool_common_error

module Utils = struct
  include Pool_common_utils

  let pool_to_ctx pool = [ "pool", Entity.Database.Label.value pool ]
end

module Database = struct
  include Database

  let root = Label.of_string "root"
end
