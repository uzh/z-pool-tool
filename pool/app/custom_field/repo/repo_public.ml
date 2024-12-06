module Database = Database
module Dynparam = Database.Dynparam

let get_field_type m = m.Repo_entity.Public.field_type
let id m = m.Repo_entity.Public.id

let has_options m =
  Entity.FieldType.(
    equal Select (get_field_type m) || equal MultiSelect (get_field_type m))
;;

let get_options pool m =
  if has_options m
  then Repo_option.Public.find_by_field pool (id m)
  else Lwt.return []
;;

let get_options_of_multiple pool fields =
  fields
  |> CCList.filter_map (fun m ->
    if has_options m then Some m.Repo_entity.Public.id else None)
  |> Repo_option.Public.find_by_multiple_fields pool
;;

let to_grouped_public is_admin pool model fields =
  let%lwt groups = Repo_group.find_by_model pool model in
  let%lwt options = get_options_of_multiple pool fields in
  Repo_entity.Public.to_grouped_entities is_admin options groups fields
  |> Lwt.return
;;

let to_ungrouped_entities pool is_admin fields =
  let%lwt options = get_options_of_multiple pool fields in
  fields
  |> Repo_entity.Public.to_ungrouped_entities is_admin options
  |> Lwt.return
;;

let base_filter_conditions is_admin =
  let base =
    {sql|
    AND pool_custom_fields.disabled = 0
    AND pool_custom_fields.published_at IS NOT NULL
  |sql}
  in
  if is_admin
  then base
  else Format.asprintf "%s AND pool_custom_fields.admin_view_only = 0 " base
;;

module Sql = struct
  let answers_left_join =
    {sql|
      LEFT JOIN pool_custom_field_answers
        ON pool_custom_field_answers.custom_field_uuid = pool_custom_fields.uuid
        AND pool_custom_field_answers.entity_uuid = UNHEX(REPLACE($1, '-', ''))
    |sql}
  ;;

  let sql_select_columns =
    [ Entity.Id.sql_select_fragment ~field:"pool_custom_fields.uuid"
    ; "pool_custom_fields.name"
    ; "pool_custom_fields.hint"
    ; "pool_custom_fields.validation"
    ; "pool_custom_fields.field_type"
    ; "pool_custom_fields.required"
    ; Entity.Id.sql_select_fragment
        ~field:"pool_custom_fields.custom_field_group_uuid"
    ; "pool_custom_fields.admin_override"
    ; "pool_custom_fields.admin_input_only"
    ; "pool_custom_fields.prompt_on_registration"
    ; Entity.Id.sql_select_fragment ~field:"pool_custom_field_answers.uuid"
    ; Entity.Id.sql_select_fragment
        ~field:"pool_custom_field_answers.entity_uuid"
    ; "pool_custom_field_answers.value"
    ; "pool_custom_field_answers.admin_value"
    ; "pool_custom_field_answers.version"
    ; "pool_custom_field_answers.admin_version"
    ]
  ;;

  let select_sql =
    Format.asprintf
      {sql|
      SELECT
        %s
      FROM pool_custom_fields
      %s
    |sql}
      (sql_select_columns |> CCString.concat ", ")
      answers_left_join
  ;;

  let find_all_by_model_request required is_admin =
    let open Caqti_request.Infix in
    let include_promted_on_registration = is_admin || required in
    let where =
      Format.asprintf
        {sql|
        WHERE pool_custom_fields.model = $2
        %s
        %s
        %s
      |sql}
        (base_filter_conditions is_admin)
        (if required then "AND pool_custom_fields.required = 1" else "")
        (if include_promted_on_registration
         then ""
         else "AND pool_custom_fields.prompt_on_registration = 0")
    in
    let order = {sql| ORDER BY pool_custom_fields.position ASC |sql} in
    Format.asprintf "%s \n %s \n %s" select_sql where order
    |> Caqti_type.(t2 Contact.Repo.Id.t string ->* Repo_entity.Public.t)
  ;;

  let find_all_by_model model ~required ~is_admin pool id =
    let open Utils.Lwt_result.Infix in
    Database.collect
      pool
      (find_all_by_model_request required is_admin)
      (id, Entity.Model.show model)
    >|> to_grouped_public is_admin pool model
  ;;

  let find_unanswered_required_by_model_request is_admin =
    let open Caqti_request.Infix in
    let where =
      Format.asprintf
        {sql|
          WHERE pool_custom_fields.model = $2
          %s
          AND pool_custom_fields.required = 1
          AND pool_custom_field_answers.value IS NULL
        |sql}
        (base_filter_conditions is_admin)
    in
    let order = {sql| ORDER BY pool_custom_fields.position ASC |sql} in
    Format.asprintf "%s \n %s \n %s" select_sql where order
    |> Caqti_type.(t2 Contact.Repo.Id.t string ->* Repo_entity.Public.t)
  ;;

  let find_unanswered_required_by_model model ~is_admin pool id =
    let open Utils.Lwt_result.Infix in
    Database.collect
      pool
      (find_unanswered_required_by_model_request is_admin)
      (id, Entity.Model.show model)
    >|> to_grouped_public is_admin pool model
  ;;

  let find_unanswered_ungrouped_required_by_model model ~is_admin pool id =
    let open Utils.Lwt_result.Infix in
    Database.collect
      pool
      (find_unanswered_required_by_model_request is_admin)
      (id, Entity.Model.show model)
    >|> to_ungrouped_entities pool is_admin
  ;;

  let find_multiple_by_contact_request is_admin ids =
    let where =
      Format.asprintf
        {sql|
        WHERE pool_custom_fields.model = $2
        %s
        AND pool_custom_fields.uuid in ( %s )
      |sql}
        (base_filter_conditions is_admin)
        (CCList.mapi
           (fun i _ -> Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + 3))
           ids
         |> CCString.concat ",")
    in
    Format.asprintf "%s \n %s" select_sql where
  ;;

  let find_multiple_by_contact ?(is_admin = false) pool contact_id ids =
    if CCList.is_empty ids
    then Lwt.return []
    else
      let open Utils.Lwt_result.Infix in
      let open Caqti_request.Infix in
      let dyn =
        let base =
          Dynparam.(
            empty
            |> add Contact.Repo.Id.t contact_id
            |> add Caqti_type.string Entity.Model.(show Contact))
        in
        CCList.fold_left
          (fun dyn id -> dyn |> Dynparam.add Contact.Repo.Id.t id)
          base
          ids
      in
      let (Dynparam.Pack (pt, pv)) = dyn in
      let request =
        find_multiple_by_contact_request is_admin ids
        |> pt ->* Repo_entity.Public.t
      in
      Database.collect pool request pv
      >|> fun fields ->
      let%lwt options = get_options_of_multiple pool fields in
      fields
      |> Repo_entity.Public.to_ungrouped_entities is_admin options
      |> Lwt.return
  ;;

  let find_by_contacts_and_fields_request contact_ids field_ids =
    let id_list counter ids =
      CCList.mapi
        (fun i _ ->
           Format.asprintf "UNHEX(REPLACE($%n, '-', ''))" (i + counter + 1))
        ids
      |> CCString.concat ","
    in
    Format.asprintf
      {sql|
        SELECT %s
        FROM pool_custom_fields
        LEFT JOIN pool_custom_field_answers
          ON pool_custom_field_answers.custom_field_uuid = pool_custom_fields.uuid
        WHERE
          pool_custom_fields.uuid in ( %s )
          AND pool_custom_field_answers.entity_uuid in ( %s )
        ORDER BY
          pool_custom_fields.position
      |sql}
      (CCString.concat ", " sql_select_columns)
      (id_list 0 field_ids)
      (id_list (CCList.length field_ids) contact_ids)
  ;;

  let find_by_contacts_and_view pool is_admin contact_ids view =
    let open Utils.Lwt_result.Infix in
    let open Caqti_request.Infix in
    if CCList.is_empty contact_ids
    then Lwt.return []
    else
      Repo.Sql.find_by_table_view pool view
      >|> function
      | [] -> Lwt.return []
      | fields ->
        let field_ids = CCList.map Entity.id fields in
        let dyn =
          let add_ids decode ids dyn =
            CCList.fold_left
              (fun dyn id ->
                 dyn |> Dynparam.add Caqti_type.string (id |> decode))
              dyn
              ids
          in
          Dynparam.empty
          |> add_ids Entity.Id.value field_ids
          |> add_ids Contact.Id.value contact_ids
        in
        let (Dynparam.Pack (pt, pv)) = dyn in
        let request =
          find_by_contacts_and_fields_request contact_ids field_ids
          |> pt ->* Repo_entity.Public.t
        in
        Database.collect pool request pv
        >|> fun fields ->
        let%lwt options = get_options_of_multiple pool fields in
        fields
        |> Repo_entity.Public.to_ungrouped_entities is_admin options
        |> Lwt.return
  ;;

  let find_by_contact_request is_admin =
    let open Caqti_request.Infix in
    Format.asprintf
      {sql|%s
      WHERE pool_custom_fields.model = $2
      %s
      AND pool_custom_fields.uuid = UNHEX(REPLACE($3, '-', ''))
    |sql}
      select_sql
      (base_filter_conditions is_admin)
    |> Caqti_type.(t3 Contact.Repo.Id.t string Repo_entity.Id.t)
       ->! Repo_entity.Public.t
  ;;

  let find_by_contact ?(is_admin = false) pool contact_id field_id =
    let open Utils.Lwt_result.Infix in
    Database.find_opt
      pool
      (find_by_contact_request is_admin)
      (contact_id, Entity.Model.(show Contact), field_id)
    ||> CCOption.to_result Pool_message.(Error.NotFound Field.CustomField)
    >>= fun field ->
    let%lwt options = get_options pool field in
    Repo_entity.Public.to_ungrouped_entities is_admin options [ field ]
    |> CCList.head_opt
    |> CCOption.to_result Pool_message.(Error.NotFound Field.CustomField)
    |> Lwt_result.lift
  ;;

  let all_answered_request ~required_only =
    let open Caqti_request.Infix in
    let base =
      Format.asprintf
        {sql|
      SELECT count(*) questions FROM pool_custom_fields
      %s
      WHERE pool_custom_fields.model = $2
      %s
      AND pool_custom_fields.prompt_on_registration = 0
      AND pool_custom_fields.field_type != $3
      AND pool_custom_fields.admin_input_only = 0
      AND pool_custom_field_answers.value IS NULL
      |sql}
        answers_left_join
        (base_filter_conditions false)
    in
    (match required_only with
     | false -> base
     | true -> Format.asprintf "%s AND pool_custom_fields.required = 1" base)
    |> Caqti_type.(
         t3 Contact.Repo.Id.t Repo_entity.Model.t Repo_entity.FieldType.t
         ->! int)
  ;;

  let all_answered ~required_only pool contact_id =
    let open Utils.Lwt_result.Infix in
    Database.find
      pool
      (all_answered_request ~required_only)
      Entity.(contact_id, Model.Contact, FieldType.Boolean)
    ||> CCInt.equal 0
  ;;

  let all_prompted_on_registration_request =
    let open Caqti_request.Infix in
    let where =
      Format.asprintf
        {sql|
          WHERE pool_custom_fields.prompt_on_registration = 1
          %s
        |sql}
        (base_filter_conditions false)
    in
    let order = {sql| ORDER BY pool_custom_fields.position ASC |sql} in
    Format.asprintf "%s \n %s \n %s" select_sql where order
    |> Caqti_type.(string ->* Repo_entity.Public.t)
  ;;

  let all_prompted_on_registration pool =
    let open Utils.Lwt_result.Infix in
    Database.collect
      pool
      all_prompted_on_registration_request
      Entity.Model.(show Contact)
    >|> to_ungrouped_entities pool false
  ;;
end

let find_all_by_contact = Sql.find_all_by_model Entity.Model.Contact

let find_unanswered_required_by_contact =
  Sql.find_unanswered_required_by_model Entity.Model.Contact
;;

let find_unanswered_ungrouped_required_by_contact =
  Sql.find_unanswered_ungrouped_required_by_model Entity.Model.Contact
;;

let find_multiple_by_contact = Sql.find_multiple_by_contact
let find_by_contact = Sql.find_by_contact
let all_required_answered = Sql.all_answered ~required_only:true
let all_answered = Sql.all_answered ~required_only:false
let all_prompted_on_registration = Sql.all_prompted_on_registration
