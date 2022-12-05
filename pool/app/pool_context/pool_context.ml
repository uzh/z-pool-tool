include Entity

(* Logging *)
let show_log_user = function
  | Admin user | Root user -> user.Sihl_user.email
  | Contact contact -> contact.Contact.user.Sihl_user.email
;;

let show_log (t : t) =
  Format.sprintf
    "%s %s"
    (Option.value ~default:"anonymous" @@ Option.map show_log_user t.user)
    (Pool_database.Label.show t.tenant_db)
;;
