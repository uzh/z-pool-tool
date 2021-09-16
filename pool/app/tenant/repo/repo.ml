open Entity
module RepoEntity = Repo_entity

module Sql = struct
  let insert_sql =
    {sql|
        INSERT INTO tenant (
          uuid,
          title,
          description,
          database,
          smtp_server,
          smtp_port,
          smtp_username,
          smtp_authentication_method,
          smtp_protocol,
          styles,
          logos,
          partner_logos,
          mainenance,
          disabled,
          created_at,
          updated_at
      |sql}
  ;;

  let insert_request = Caqti_request.exec RepoEntity.t insert_sql
  let insert = Utils.Database.exec insert_request
end

let find_by_id (id : string) : (t, string) result Lwt.t = Utils.todo id
let list_all = Utils.todo
let insert = Sql.insert
let update (t : t) = Utils.todo t
let destroy = Utils.todo
