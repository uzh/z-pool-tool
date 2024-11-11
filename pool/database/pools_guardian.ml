module Make (Config : Pools_sig.ConfigSig) : Guardian_backend.Pools.Sig = struct
  include Pools.Make (Config)

  let of_ctx ctx =
    CCOption.bind ctx (CCList.assoc_opt ~eq:CCString.equal "pool")
    |> CCOption.get_exn_or "Invalid ctx"
    |> Entity.Label.of_string
  ;;

  let initialize ?additinal_pools =
    let additinal_pools =
      CCOption.map (CCList.map (CCFun.uncurry Entity.create)) additinal_pools
    in
    Pool.initialize ?additinal_pools
  ;;

  let connect =
    let open CCFun.Infix in
    Pool.connect %> CCResult.map_err Pool_message.Error.show
  ;;

  let disconnect = Pool.disconnect

  let add_pool ?required name database_url =
    Entity.create name database_url |> Pool.add ?required
  ;;

  let drop_pool = Pool.drop
  let fetch_pool ?ctx ?retries () = Pool.fetch ?retries (of_ctx ctx)
  let find ?ctx = find (of_ctx ctx)
  let find_opt ?ctx = find_opt (of_ctx ctx)
  let collect ?ctx = collect (of_ctx ctx)
  let exec ?ctx = exec (of_ctx ctx)
  let populate ?ctx = populate (of_ctx ctx)

  let transaction ?ctx ?setup ?cleanup =
    transaction ?setup ?cleanup (of_ctx ctx)
  ;;

  let transaction_iter ?ctx = transaction_iter (of_ctx ctx)
end
