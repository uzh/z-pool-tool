(* TODO: Once the guardian package has a cached version, this implementation can
   be updated/removed (Issue: https://github.com/uzh/guardian/issues/11) *)
include
  Guardian_backend.MariaDb.Make (Role.Actor) (Role.Target)
    (Pool_database.GuardBackend)

let src = Logs.Src.create "guard"

module Cache = struct
  open CCCache

  let equal_find_actor (l1, r1, a1) (l2, r2, a2) =
    Pool_database.Label.equal l1 l2
    && Role.equal r1 r2
    && Uuid.Actor.equal a1 a2
  ;;

  let equal_validation (l1, s1, any1, a1) (l2, s2, any2, a2) =
    Pool_database.Label.equal l1 l2
    && Core.ValidationSet.equal s1 s2
    && CCBool.equal any1 any2
    && Core.Actor.(Uuid.Actor.equal (id a1) (id a2))
  ;;

  let lru_find_actor = lru ~eq:equal_find_actor 2048
  let lru_validation = lru ~eq:equal_validation 2048

  let clear () =
    let () = clear lru_validation in
    clear lru_find_actor
  ;;
end

module Actor = struct
  include Actor

  let find database_label typ id =
    let cb ~in_cache _ _ =
      if in_cache
      then (
        let tags = Pool_database.Logger.Tags.create database_label in
        Logs.debug ~src (fun m ->
          m ~tags "Found in cache: Actor %s" (id |> Uuid.Actor.to_string)))
      else ()
    in
    let find' (label, typ, id) =
      find ~ctx:(Pool_database.to_ctx label) typ id
    in
    (database_label, typ, id)
    |> CCCache.(with_cache ~cb Cache.lru_find_actor find')
  ;;
end

let validate
  ?(any_id = false)
  database_label
  validation_set
  (actor : Role.t Core.Actor.t)
  =
  let cb ~in_cache _ _ =
    if in_cache
    then
      Logs.debug ~src (fun m ->
        m
          ~tags:(Pool_database.Logger.Tags.create database_label)
          "Found in cache: Actor %s\nValidation set %s"
          (actor |> Core.Actor.id |> Uuid.Actor.to_string)
          ([%show: Core.ValidationSet.t] validation_set))
    else ()
  in
  let validate' (label, set, any_id, actor) =
    validate
      ~ctx:(Pool_database.to_ctx label)
      ~any_id
      Pool_common.Message.authorization
      set
      actor
  in
  (database_label, validation_set, any_id, actor)
  |> CCCache.(with_cache ~cb Cache.lru_validation validate')
;;
