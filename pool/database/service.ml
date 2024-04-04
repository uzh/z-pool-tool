open Entity
module Guard = Guardian_backend.Pools.Make (Entity.MariaConfig)
include Core.Make (Pools.Make (Entity.MariaConfigPool))

let add_pool model =
  let pool_size =
    Sihl.Configuration.read_string "DATABASE_POOL_SIZE"
    |> CCFun.flip CCOption.bind CCInt.of_string
    |> CCOption.value ~default:10
  in
  let () = Guard.add_pool ~pool_size model.label model.url in
  add_pool ~pool_size model.label model.url
;;

let drop_pool label =
  let%lwt () = Guard.drop_pool label in
  drop_pool label
;;
