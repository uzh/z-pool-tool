let migration () =
  let open Database.Migration in
  ()
  |> Guard.Persistence.find_migrations
  |> CCList.map (fun (key, date, sql) ->
    let new_step = Step.create ~label:(Format.asprintf "%s_%s" date key) sql in
    let ptime_to_human time =
      let (year, month, day), ((hour, minute, _), _) = Ptime.to_date_time time in
      Format.asprintf "%i%02i%02i%02i%02i_guardian" year month day hour minute
    in
    let fst (time, _, _) = time in
    let migration_name =
      CCResult.catch
        (Ptime.of_rfc3339 ~sub:true date |> CCResult.map fst)
        ~ok:ptime_to_human
        ~err:(function
          | `RFC3339 (_, `Eoi) ->
            Format.asprintf "%s:00-00:00" date
            |> Ptime.of_rfc3339
            |> Ptime.rfc3339_string_error
            |> CCResult.get_or_failwith
            |> fst
            |> ptime_to_human
          | `RFC3339 (_, err) -> [%show: Ptime.rfc3339_error] err |> failwith)
    in
    empty migration_name |> add_step new_step)
;;
