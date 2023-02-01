include Entity

let is_from_root { database_label; _ } = Pool_database.is_root database_label

let user_is_admin = function
  | Guest | Contact _ -> false
  | Admin _ -> true
;;
