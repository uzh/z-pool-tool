include Entity

let is_from_root { database_label; _ } =
  Pool_database.(Label.equal database_label root)
;;
