open Migrations

let sort = CCList.stable_sort (fun a b -> CCString.compare (fst a) (fst b))

let steps =
  let sorted =
    Migration_authorization.migration ()
    @ [ Migration_202303211734.migration ()
      ; Migration_202303230956.migration ()
      ; Migration_202303291025.migration ()
      ; Migration_202305151556.migration ()
      ; Migration_202306021512.migration ()
      ; Migration_202306071352.migration ()
      ; Migration_202306081615.migration ()
      ; Migration_202306261642.migration ()
      ; Migration_202307081414.migration ()
      ; Migration_202307121006.migration ()
      ; Migration_202307121330.migration ()
      ; Migration_202307121722.migration ()
      ; Migration_202307131619.migration ()
      ; Migration_202307250839.migration ()
      ; Migration_202307281144.migration ()
      ; Migration_202308021016.migration ()
      ; Migration_202308030850.migration ()
      ; Migration_202308041330.migration ()
      ; Migration_202308210946.migration ()
      ; Migration_202309081700.migration ()
      ; Migration_202309121645.migration ()
      ; Migration_202309180820.migration ()
      ; Migration_202309190858.migration ()
      ; Migration_202309211305.migration ()
      ; Migration_202309251059.migration ()
      ; Migration_202309261013.migration ()
      ; Migration_202309281435.migration ()
      ; Migration_202310110929.migration ()
      ; Migration_202310191345.migration ()
      ; Migration_202311011626.migration ()
      ; Migration_202312040852.migration ()
      ; Migration_202312121047.migration ()
      ; Migration_202312150920.migration ()
      ; Migration_202312200952.migration ()
      ; Migration_202312201325.migration ()
      ; Migration_202401081554.migration ()
      ; Migration_202401120946.migration ()
      ; Migration_202401191048.migration ()
      ; Migration_202401221730.migration ()
      ; Migration_202402081615.migration ()
      ; Migration_202402091434.migration ()
      ; Migration_202402150942.migration ()
      ; Migration_202402161523.migration ()
      ; Migration_202402201330.migration ()
      ; Migration_202403131128.migration ()
      ; Migration_202403181624.migration ()
      ]
    |> sort
  in
  [ Migration_person.migration ()
  ; Migration_contact.migration ()
  ; Migration_email_address.migration ()
  ; Migration_settings.migration ()
  ; Migration_i18n.migration ()
  ; Migration_assignment.migration ()
  ; Migration_session.migration ()
  ; Migration_invitation.migration ()
  ; Migration_experiment.migration ()
  ; Migration_waiting_list.migration ()
  ; Migration_location.migration ()
  ; Migration_location_file_mapping.migration ()
  ; Migration_mailing.migration ()
  ; Migration_filter.migration ()
  ; Migration_custom_fields.migration ()
  ; Migration_custom_field_answers.migration ()
  ; Migration_custom_field_options.migration ()
  ; Migration_custom_field_groups.migration ()
  ; Migration_custom_field_answer_versions.migration ()
  ; Migration_message_templates.migration ()
  ; Migration_tenant.migration_tenant ()
  ]
  @ sorted
  |> Database.Migration.extend_migrations
;;

let start () =
  let open Database in
  let tags = Logger.Tags.create root in
  let%lwt db_pools = Database.Tenant.find_all_running () in
  Lwt_list.iter_s
    (fun pool ->
      Logs.info (fun m -> m ~tags "Migrate database: %a" Label.pp pool);
      Migration.check_migrations_status pool ~migrations:(steps ()) ())
    db_pools
;;

let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "migration.tenants"
    ~dependencies:(fun () ->
      [ Database.Tenant.lifecycle; Pool_token.lifecycle ])
    ~start
    ~stop
;;

let register () = Sihl.Container.Service.create lifecycle
