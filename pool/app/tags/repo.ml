open Utils.Lwt_result.Infix
open Entity
module Label = Pool_database.Label

module Entity = struct
  module Id = struct
    include Entity.Id
    include Pool_common.Repo.Id
  end

  module Title = struct
    include Title

    let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
  end

  module Description = struct
    include Description

    let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
  end

  module Model = Pool_common.Repo.Model.SelectorType (Model)
end

let t =
  let encode (m : t) = Ok (m.id, (m.title, (m.description, m.model))) in
  let decode (id, (title, (description, model))) : (t, string) result =
    Ok { id; title; description; model }
  in
  let open Entity in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2 Id.t (tup2 Title.t (tup2 (option Description.t) Model.t))))
;;

module Tagged = struct
  include Tagged

  let t =
    let encode m = Ok (m.model_uuid, m.tag_uuid) in
    let decode (model_uuid, tag_uuid) = Ok { model_uuid; tag_uuid } in
    let open Entity in
    Caqti_type.(custom ~encode ~decode (tup2 Pool_common.Repo.Id.t Id.t))
  ;;
end

module Sql = struct
  open Caqti_request.Infix
  module Dynparam = Utils.Database.Dynparam

  let select_tag_sql =
    {sql|
      SELECT
        LOWER(CONCAT(
          SUBSTR(HEX(pool_tags.uuid), 1, 8), '-',
          SUBSTR(HEX(pool_tags.uuid), 9, 4), '-',
          SUBSTR(HEX(pool_tags.uuid), 13, 4), '-',
          SUBSTR(HEX(pool_tags.uuid), 17, 4), '-',
          SUBSTR(HEX(pool_tags.uuid), 21)
        )),
        pool_tags.title,
        pool_tags.description,
        pool_tags.model
      FROM
        pool_tags
    |sql}
  ;;

  let find_request =
    {sql|
      WHERE
        pool_tags.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Format.asprintf "%s\n%s" select_tag_sql
    |> Entity.Id.t ->! t
  ;;

  let find pool id =
    Utils.Database.find_opt (Label.value pool) find_request id
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Tag)
  ;;

  let find_all_request =
    Format.asprintf
      {sql|
        %s
        ORDER BY pool_tags.title, pool_tags.model
      |sql}
      select_tag_sql
    |> Caqti_type.unit ->* t
  ;;

  let find_all pool =
    Utils.Database.collect (Label.value pool) find_all_request ()
  ;;

  let find_all_with_model_request =
    Format.asprintf
      {sql|
        %s
        WHERE pool_tags.model = ?
        ORDER BY pool_tags.title, pool_tags.model
      |sql}
      select_tag_sql
    |> Entity.Model.t ->* t
  ;;

  let find_all_with_model pool model =
    Utils.Database.collect (Label.value pool) find_all_with_model_request model
  ;;

  let already_exists_request ?exclude_id () =
    let open Entity in
    let open CCFun.Infix in
    let without_uuid =
      CCOption.map_or
        ~default:""
        (Id.value
         %> Format.asprintf
              {|AND pool_tags.uuid != UNHEX(REPLACE('%s', '-', ''))|})
        exclude_id
    in
    Format.asprintf
      {sql|
        SELECT TRUE
        FROM pool_tags
        WHERE pool_tags.title = ?
          AND pool_tags.model = ?
          %s
      |sql}
      without_uuid
    |> Caqti_type.(tup2 Title.t Model.t ->! bool)
  ;;

  let already_exists pool ?exclude_id title model =
    Utils.Database.find_opt
      (Label.value pool)
      (already_exists_request ?exclude_id ())
      (title, model)
    ||> CCOption.value ~default:false
  ;;

  let find_all_validated_request =
    let open Guard.Persistence in
    Format.asprintf
      {sql|
        %s
        GROUP BY pool_tags.uuid
        HAVING guardianValidateTagUuid(guardianEncodeUuid(?), ?, pool_tags.uuid)
        ORDER BY pool_tags.title, pool_tags.model
      |sql}
      select_tag_sql
    |> Caqti_type.(tup2 Uuid.Actor.t Action.t) ->* t
  ;;

  let find_all_validated ?(action = Guard.Action.Read) pool actor =
    Utils.Database.collect
      (Label.value pool)
      find_all_validated_request
      (Guard.Actor.id actor, action)
  ;;

  let find_all_validated_with_model_request =
    let open Guard.Persistence in
    Format.asprintf
      {sql|
        %s
        WHERE pool_tags.model = ?
        GROUP BY pool_tags.uuid
        HAVING guardianValidateTagUuid(guardianEncodeUuid(?), ?, pool_tags.uuid)
        ORDER BY pool_tags.title, pool_tags.model
      |sql}
      select_tag_sql
    |> Caqti_type.(tup3 Entity.Model.t Uuid.Actor.t Action.t) ->* t
  ;;

  let find_all_validated_with_model
    ?(action = Guard.Action.Read)
    pool
    model
    actor
    =
    Utils.Database.collect
      (Label.value pool)
      find_all_validated_with_model_request
      (model, Guard.Actor.id actor, action)
  ;;

  let insert_request =
    {sql|
      INSERT INTO pool_tags (
        uuid,
        title,
        description,
        model
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        $3,
        $4
      )
    |sql}
    |> t ->. Caqti_type.unit
  ;;

  let insert pool tag =
    try
      Utils.Database.exec (Label.value pool) insert_request tag |> Lwt_result.ok
    with
    | _ -> Lwt.return_error Pool_common.Message.(Invalid Field.Tag)
  ;;

  let update_request =
    let open Caqti_request.Infix in
    {sql|
      UPDATE pool_tags
      SET
        title = $2,
        description = $3,
        model = $4
      WHERE
        pool_tags.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(t ->. unit)
  ;;

  let update pool = Utils.Database.exec (Label.value pool) update_request

  let delete_request =
    {sql|
      DELETE FROM pool_tags
      WHERE uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Entity.Id.t ->. Caqti_type.unit
  ;;

  let delete pool ({ id; _ } : t) =
    Utils.Database.exec (Label.value pool) delete_request id
  ;;

  module Tagged = struct
    let insert_request =
      {sql|
        INSERT INTO pool_tagging (
          model_uuid,
          tag_uuid
        ) VALUES (
          UNHEX(REPLACE($1, '-', '')),
          UNHEX(REPLACE($2, '-', ''))
        )
      |sql}
      |> Tagged.t ->. Caqti_type.unit
    ;;

    let insert pool tagging =
      try
        Utils.Database.exec (Label.value pool) insert_request tagging
        |> Lwt_result.ok
      with
      | _ -> Lwt.return_error Pool_common.Message.(Invalid Field.Tagging)
    ;;

    let delete_request =
      {sql|
        DELETE FROM pool_tagging
        WHERE model_uuid = UNHEX(REPLACE(?, '-', ''))
          AND tag_uuid = UNHEX(REPLACE(?, '-', ''))
      |sql}
      |> Tagged.t ->. Caqti_type.unit
    ;;

    let delete pool tagging =
      Utils.Database.exec (Label.value pool) delete_request tagging
    ;;

    let select_tagging_sql =
      {sql|
        SELECT
          LOWER(CONCAT(
            SUBSTR(HEX(pool_tagging.model_uuid), 1, 8), '-',
            SUBSTR(HEX(pool_tagging.model_uuid), 9, 4), '-',
            SUBSTR(HEX(pool_tagging.model_uuid), 13, 4), '-',
            SUBSTR(HEX(pool_tagging.model_uuid), 17, 4), '-',
            SUBSTR(HEX(pool_tagging.model_uuid), 21)
          )),
          LOWER(CONCAT(
            SUBSTR(HEX(pool_tagging.tag_uuid), 1, 8), '-',
            SUBSTR(HEX(pool_tagging.tag_uuid), 9, 4), '-',
            SUBSTR(HEX(pool_tagging.tag_uuid), 13, 4), '-',
            SUBSTR(HEX(pool_tagging.tag_uuid), 17, 4), '-',
            SUBSTR(HEX(pool_tagging.tag_uuid), 21)
          ))
        FROM
          pool_tagging
      |sql}
    ;;

    let join_tags =
      {sql|JOIN pool_tags ON pool_tagging.tag_uuid = pool_tags.uuid|sql}
    ;;

    let create_find_all_tag_sql select_from_model join_model_tablename =
      Format.asprintf
        {sql|
          %s
          JOIN pool_tagging ON %s.uuid = pool_tagging.model_uuid
          %s
          WHERE
            pool_tags.model = ? AND pool_tags.title = ?
        |sql}
        select_from_model
        join_model_tablename
        join_tags
    ;;

    let find_all_of_entity_request =
      let open Entity in
      Format.asprintf
        {sql|
          %s
          JOIN pool_tagging ON pool_tags.uuid = pool_tagging.tag_uuid
          WHERE pool_tags.model = ?
            AND pool_tagging.model_uuid = UNHEX(REPLACE(?, '-', ''))
        |sql}
        select_tag_sql
      |> Caqti_type.(tup2 Model.t Pool_common.Repo.Id.t ->* t)
    ;;

    let find_all_of_entity pool model id =
      Utils.Database.collect
        (Label.value pool)
        find_all_of_entity_request
        (model, id)
    ;;
  end
end

let find = Sql.find
let find_all = Sql.find_all
let find_all_with_model = Sql.find_all_with_model
let find_all_validated = Sql.find_all_validated
let find_all_validated_with_model = Sql.find_all_validated_with_model
let find_all_of_entity = Sql.Tagged.find_all_of_entity
let create_find_all_tag_sql = Sql.Tagged.create_find_all_tag_sql
let already_exists = Sql.already_exists
let insert = Sql.insert
let delete = Sql.delete
let insert_tagged = Sql.Tagged.insert
let delete_tagged = Sql.Tagged.delete
let update = Sql.update
