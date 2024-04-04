let setup_databases () =
  let%lwt (_ : Database.status) = Pool_database.Root.setup () in
  Pool_database.Tenant.setup ()
;;

let failwith_missmatch help =
  print_endline help;
  failwith "Argument missmatch"
;;

let is_available_exn ?(include_root = false) pool =
  let open Database in
  let pool = Label.create pool |> Pool_common.Utils.get_or_failwith in
  let%lwt (_ : status) = Pool_database.Root.setup () in
  let%lwt pools = Pool_database.Tenant.setup () in
  let available_pools = if include_root then root :: pools else pools in
  if CCList.mem ~eq:Label.equal pool available_pools
  then Lwt.return pool
  else
    failwith
      (Format.asprintf
         "The specified database pool %s is not available (%s)."
         ([%show: Label.t] pool)
         ([%show: Label.t list] available_pools))
;;

let make_no_args name description fcn =
  let help = Format.asprintf {|

Example: %s
  |} name in
  Sihl.Command.make ~name ~description ~help (function
    | [] -> fcn ()
    | _ -> failwith_missmatch help)
;;

let make_pool_specific name description fcn =
  let help =
    Format.asprintf
      {|<database_label>

Provide the following variables:
        <database_label>      : string

Example: %s econ-uzh
  |}
      name
  in
  Sihl.Command.make ~name ~description ~help (function
    | [ pool ] ->
      let%lwt pool = is_available_exn pool in
      fcn pool
    | _ -> failwith_missmatch help)
;;

let encrypt_string =
  let name = "utils.encrypt.aes" in
  let description = "Utility function, encrypt a string with AES (CTR)." in
  let help =
    Format.asprintf
      {|<string>

Provide the following variables:
        <string>      : string to encrypt

Example: %s hello world
  |}
      name
  in
  Sihl.Command.make ~name ~description ~help (fun decrypted ->
    let encrypted =
      Utils.Crypto.String.encrypt_to_string (CCString.concat " " decrypted)
    in
    let () = print_endline (Format.asprintf "Encrypted: %s" encrypted) in
    Lwt.return_some ())
;;
