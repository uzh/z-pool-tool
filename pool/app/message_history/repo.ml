open Entity

let create_caqti_type =
  let open Pool_common.Repo in
  let encode m =
    Ok
      ( Pool_common.Id.of_string m.job.Sihl_queue.id
      , m.entity_uuid
      , m.message_template )
  in
  let decode _ = failwith "Write only model" in
  Caqti_type.(custom ~encode ~decode (t3 Id.t Id.t (option string)))
;;

let insert_request =
  let open Caqti_request.Infix in
  {sql|
      INSERT INTO message_history (
        queue_job_uuid,
        entity_uuid,
        message_template
      ) VALUES (
        UNHEX(REPLACE($1, '-', '')),
        UNHEX(REPLACE($2, '-', '')),
        $3
      )
    |sql}
  |> Caqti_type.(create_caqti_type ->. unit)
;;

let insert pool t =
  Utils.Database.exec (Pool_database.Label.value pool) insert_request t
;;
