include Entity
module Repo = Repo

module Database = struct
  include Database

  let root = Label.of_string "root"
end
