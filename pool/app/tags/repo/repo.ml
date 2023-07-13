open Utils.Lwt_result.Infix
open Entity
module Label = Pool_database.Label

module Entity = struct
  module Id = Pool_common.Repo.Id

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
  let encode (m : t) = Ok (m.id, m.title, m.description) in
  let decode (id, title, description) : (t, string) result =
    Ok { id; title; description }
  in
  let open Entity in
  Caqti_type.(custom ~encode ~decode (tup3 Id.t Title.t (option Description.t)))
;;

module Tagged = struct
  include Tagged

  let t =
    let encode m = Ok (m.model, m.model_uuid, m.tag_uuid) in
    let decode (model, model_uuid, tag_uuid) =
      Ok { model; model_uuid; tag_uuid }
    in
    let open Entity in
    Caqti_type.(
      custom ~encode ~decode (tup3 Model.t Pool_common.Repo.Id.t Id.t))
  ;;
end

module Sql = struct
  open Caqti_request.Infix
  module Dynparam = Utils.Database.Dynparam

  let select_sql =
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
        pool_tags.description
      FROM
        pool_tags
    |sql}
  ;;

  let find_request =
    {sql|
      WHERE
        pool_tags.uuid = UNHEX(REPLACE(?, '-', ''))
    |sql}
    |> Format.asprintf "%s\n%s" select_sql
    |> Entity.Id.t ->! t
  ;;

  let find pool id =
    Utils.Database.find_opt (Label.value pool) find_request id
    ||> CCOption.to_result Pool_common.Message.(NotFound Field.Tag)
  ;;

  let find_all_request = select_sql |> Caqti_type.unit ->* t

  let find_all pool =
    Utils.Database.collect (Label.value pool) find_all_request ()
  ;;

  let insert_request =
    {sql|
      INSERT INTO pool_tags (
        uuid,
        title,
        description
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        $2,
        $3
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
    let open Entity in
    {sql|
      UPDATE pool_tags
      SET
        title = $2,
        description = $3
      WHERE
        pool_tags.uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
    |> Caqti_type.(tup3 Id.t Title.t (option Description.t) ->. unit)
  ;;

  let update pool ({ id; title; description; _ } : t) =
    Utils.Database.exec
      (Label.value pool)
      update_request
      (id, title, description)
  ;;

  module Tagged = struct
    let insert_request =
      {sql|
        INSERT INTO pool_tagging (
          model,
          model_uuid,
          tag_uuid
        ) VALUES (
          $1,
          UNHEX(REPLACE($2, '-', '')),
          UNHEX(REPLACE($3, '-', ''))
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
        WHERE model = ?
          AND model_uuid = UNHEX(REPLACE(?, '-', ''))
          AND tag_uuid = UNHEX(REPLACE(?, '-', ''))
      |sql}
      |> Tagged.t ->. Caqti_type.unit
    ;;

    let delete pool tagging =
      Utils.Database.exec (Label.value pool) delete_request tagging
    ;;

    let select_sql =
      {sql|
        SELECT
          pool_tagging.model
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

    let find_all_models_by_tag_sql model select_from_model join_model_tablename =
      Format.asprintf
        {sql|
          %s
          JOIN pool_tagging ON %s.uuid = pool_tagging.model_uuid
          %s
          WHERE
            pool_tagging.model = '%s' AND pool_tags.title = ?
        |sql}
        select_from_model
        join_model_tablename
        join_tags
        (Model.show model)
    ;;

    let find_all_by_model_and_tag_request =
      let open Entity in
      Format.asprintf
        {sql|
          %s
          %s
          WHERE
            pool_tagging.model = ? AND pool_tags.title = ?
        |sql}
        select_sql
        join_tags
      |> Caqti_type.(tup2 Model.t Title.t) ->* Tagged.t
    ;;

    let find_all_by_model_and_tag pool model (tag : t) =
      Utils.Database.collect
        (Label.value pool)
        find_all_by_model_and_tag_request
        (model, tag.title)
    ;;
  end
end

let find = Sql.find
let find_all = Sql.find_all
let find_all_models_by_tag_sql = Sql.Tagged.find_all_models_by_tag_sql
let insert = Sql.insert
let insert_tagged = Sql.Tagged.insert
let delete_tagged = Sql.Tagged.delete
let update = Sql.update
