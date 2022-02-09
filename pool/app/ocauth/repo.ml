module MariaConfig = struct
  let connection_string =
    let str = Sys.getenv "DATABASE_URL" in
    Logs.info (fun m -> m "Using database connection string %s" str);
    str
  ;;
end

include Ocaml_authorize_backends.Mariadb_backend.Make (Role) (MariaConfig) ()
