let migration () =
  ()
  |> Guard.Persistence.find_migrations
  |> CCList.fold_left
       (fun init (key, date, sql) ->
         let open Sihl.Database.Migration in
         let new_step =
           create_step ~label:(Format.asprintf "%s_%s" date key) sql
         in
         init |> add_step new_step)
       (Sihl.Database.Migration.empty "guardian")
;;
