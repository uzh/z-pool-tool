let migration () =
  let open Database.Migration in
  ()
  |> Guard.Persistence.find_migrations
  |> CCList.map (fun (key, date, sql) ->
    let new_step = Step.create ~label:(Format.asprintf "%s_%s" date key) sql in
    let ptime_to_human time =
      let (year, month, day), ((hour, minute, _), _) =
        Pool_model.Time.to_date_time time
      in
      Format.asprintf "%i%02i%02i%02i%02i_guardian" year month day hour minute
    in
    let fst (time, _, _) = time in
    let migration_name =
      CCResult.catch
        (Pool_model.Time.of_rfc3339 ~sub:true date |> CCResult.map fst)
        ~ok:ptime_to_human
        ~err:(function
          | `RFC3339 (_, `Eoi) ->
            Format.asprintf "%s:00-00:00" date
            |> Pool_model.Time.of_rfc3339
            |> Pool_model.Time.rfc3339_string_error
            |> CCResult.get_or_failwith
            |> fst
            |> ptime_to_human
          | `RFC3339 (_, err) -> [%show: Pool_model.Time.rfc3339_error] err |> failwith)
    in
    empty migration_name |> add_step new_step)
;;
