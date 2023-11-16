module Assets = Seed_assets
module File = Pool_common.File

let get_or_failwith = Pool_common.Utils.get_or_failwith

let create () =
  let styles = Assets.dummy_css () in
  let icon = Assets.dummy_icon () in
  let tenant_logo = Assets.dummy_tenant_logo () in
  let%lwt () =
    Lwt_list.iter_s
      (fun file ->
        let open Assets in
        let stored_file =
          Sihl_storage.
            { id = file.Assets.id
            ; filename = file.filename
            ; filesize = file.filesize
            ; mime = file.mime
            }
        in
        let base64 = Base64.encode_exn file.body in
        let%lwt _ = Service.Storage.upload_base64 stored_file base64 in
        Lwt.return_unit)
      [ styles; icon; tenant_logo ]
  in
  let data =
    let gtx_api_key = "Gtx api key" in
    if Sihl.Configuration.is_test ()
    then (
      let database_url =
        Sihl.Configuration.read_string "DATABASE_URL_TENANT_TEST"
        |> CCOption.get_exn_or "DATABASE_URL_TENANT_TEST undefined"
      in
      [ ( "Econ test"
        , "description"
        , "test.pool.econ.uzh.ch"
        , database_url
        , "econ-test"
        , gtx_api_key
        , styles.Assets.id
        , icon.Assets.id
        , "EN" )
      ])
    else
      [ ( "Econ UZH"
        , "description"
        , "localhost:3017"
        , Sihl.Configuration.read_string "DATABASE_URL_TENANT_TEST"
          |> Option.value ~default:"mariadb://root@database-tenant:3306/dev_econ"
        , "econ-uzh"
        , gtx_api_key
        , styles.Assets.id
        , icon.Assets.id
        , "EN" )
      ; ( "ZHAW"
        , "description"
        , "pool.zhaw.ch"
        , Sihl.Configuration.read_string "DATABASE_URL_TENANT_TEST_ZHAW"
          |> Option.value ~default:"mariadb://root@database-tenant:3306/dev_zhaw"
        , "zhaw"
        , gtx_api_key
        , styles.Assets.id
        , icon.Assets.id
        , "DE" )
      ]
  in
  let%lwt () =
    data
    |> Lwt_list.iter_s
         (fun
             ( title
             , description
             , url
             , database_url
             , database_label
             , gtx_api_key
             , styles
             , icon
             , default_language )
           ->
            let tenant =
              let database =
                let open Pool_tenant.Database in
                let open CCResult.Infix in
                let* url = Url.create database_url in
                let* label = Label.create database_label in
                create label url
              in
              Pool_tenant.(
                Write.create
                  (Title.create title |> get_or_failwith)
                  (Description.create description
                   |> get_or_failwith
                   |> CCOption.return)
                  (Url.create url |> get_or_failwith)
                  (database |> get_or_failwith)
                  (GtxApiKey.of_string gtx_api_key)
                  (Styles.Write.create styles
                   |> get_or_failwith
                   |> CCOption.return)
                  (Icon.Write.create icon |> get_or_failwith |> CCOption.return)
                  (Pool_common.Language.create default_language
                   |> get_or_failwith))
            in
            let logo_mappings =
              let open Pool_tenant.LogoMapping in
              [ LogoType.TenantLogo, tenant_logo.Assets.id ]
              |> CCList.map (fun (logo_type, asset_id) ->
                { Write.id = Pool_common.Id.create ()
                ; tenant_id = tenant.Pool_tenant.Write.id
                ; asset_id = Pool_common.Id.of_string asset_id
                ; logo_type
                })
            in
            [ Pool_tenant.Created tenant
            ; Pool_tenant.LogosUploaded logo_mappings
            ]
            |> Lwt_list.iter_s (Pool_tenant.handle_event Pool_database.root))
  in
  Lwt.return_unit
;;
