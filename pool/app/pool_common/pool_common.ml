include Entity
module Repo = Repo
module Utils = Pool_common_utils
module Error = Pool_common_error

module Database = struct
  include Database

  let root = Label.of_string "root"
end
