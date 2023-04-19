open CCFun
module BaseRole = Role

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

  let expand_roles : roles actor -> role_set =
    Core.Actor.roles
    %> fun set ->
    Core.RoleSet.fold
      (fun role ini ->
        role |> BaseRole.Actor.can_assign_roles |> Core.RoleSet.add_list ini)
      set
      set
  ;;

  let match_role (actor : BaseRole.Actor.t Core.Actor.t) role =
    Core.RoleSet.fold
      (fun role' ini -> ini || BaseRole.Actor.equal_or_nil_target role' role)
      (actor |> expand_roles)
      false
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

module Rule = struct
  include Rule

  let t =
    let encode =
      let open Core in
      function
      | ActorSpec.Entity arole, act, TargetSpec.Entity trole ->
        Ok (arole, (None, (act, (trole, None))))
      | ActorSpec.Id (arole, aid), act, TargetSpec.Entity trole ->
        Ok (arole, (Some aid, (act, (trole, None))))
      | ActorSpec.Entity arole, act, TargetSpec.Id (trole, tid) ->
        Ok (arole, (None, (act, (trole, Some tid))))
      | ActorSpec.Id (arole, aid), act, TargetSpec.Id (trole, tid) ->
        Ok (arole, (Some aid, (act, (trole, Some tid))))
    in
    let decode (arole, (aid, (act, (trole, tid)))) =
      let open Core in
      match aid, tid with
      | Some aid, Some tid ->
        Ok (ActorSpec.Id (arole, aid), act, TargetSpec.Id (trole, tid))
      | None, Some tid ->
        Ok (ActorSpec.Entity arole, act, TargetSpec.Id (trole, tid))
      | Some aid, None ->
        Ok (ActorSpec.Id (arole, aid), act, TargetSpec.Entity trole)
      | None, None -> Ok (ActorSpec.Entity arole, act, TargetSpec.Entity trole)
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Role.t
           (tup2
              (option Uuid.Actor.t)
              (tup2 Action.t (tup2 Kind.t (option Uuid.Target.t))))))
  ;;

  let find_all_by_actor database_label actor : Core.Rule.t list Lwt.t =
    let open Utils.Lwt_result.Infix in
    let filter_by (actor : BaseRole.Actor.t Core.Actor.t) (rules : rule list) =
      CCList.filter
        (fun (actor', _, _) ->
          match actor' with
          | Core.ActorSpec.Id (role, id) ->
            Uuid.Actor.equal id (actor |> Core.Actor.id)
            && Actor.match_role actor role
          | Core.ActorSpec.Entity role -> Actor.match_role actor role)
        rules
    in
    let open Caqti_request.Infix in
    let query =
      Format.asprintf
        {sql|
        SELECT
          actor_role,
          LOWER(CONCAT(
            SUBSTR(HEX(actor_uuid), 1, 8), '-',
            SUBSTR(HEX(actor_uuid), 9, 4), '-',
            SUBSTR(HEX(actor_uuid), 13, 4), '-',
            SUBSTR(HEX(actor_uuid), 17, 4), '-',
            SUBSTR(HEX(actor_uuid), 21)
          )),
          act,
          target_role,
          LOWER(CONCAT(
            SUBSTR(HEX(target_uuid), 1, 8), '-',
            SUBSTR(HEX(target_uuid), 9, 4), '-',
            SUBSTR(HEX(target_uuid), 13, 4), '-',
            SUBSTR(HEX(target_uuid), 17, 4), '-',
            SUBSTR(HEX(target_uuid), 21)
          ))
        FROM guardian_rules
      |sql}
      |> Caqti_type.unit ->* t
    in
    Utils.Database.collect (Pool_database.Label.value database_label) query ()
    ||> filter_by actor
  ;;
end
