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
        let%lwt _ = Storage.upload_base64 Database.root stored_file base64 in
        Lwt.return_unit)
      [ styles; icon; tenant_logo ]
  in
  let data =
    if Sihl.Configuration.is_test ()
    then (
      let database_url =
        Sihl.Configuration.read_string "DATABASE_URL_TENANT_ONE"
        |> CCOption.get_exn_or "DATABASE_URL_TENANT_ONE undefined"
      in
      [ ( "Econ test"
        , "description"
        , "test.pool.econ.uzh.ch"
        , database_url
        , "econ-test"
        , styles.Assets.id
        , icon.Assets.id
        , "EN" )
      ])
    else
      [ ( "Econ UZH"
        , "description"
        , "localhost:3017"
        , Sihl.Configuration.read_string "DATABASE_URL_TENANT_ONE"
          |> CCOption.value
               ~default:"mariadb://root@database-tenant:3306/dev_econ"
        , "econ-uzh"
        , styles.Assets.id
        , icon.Assets.id
        , "EN" )
      ; ( "ZHAW"
        , "description"
        , "pool.zhaw.ch"
        , Sihl.Configuration.read_string "DATABASE_URL_TENANT_TWO"
          |> CCOption.value
               ~default:"mariadb://root@database-tenant:3306/dev_zhaw"
        , "zhaw"
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
             , styles
             , icon
             , default_language )
           ->
            let database =
              let open Database in
              let open CCResult in
              Pool_common.Utils.get_or_failwith
              @@ (both (Label.create database_label) (Url.create database_url)
                  >|= CCFun.uncurry create)
            in
            let tenant =
              let open Pool_tenant in
              Write.create
                (Title.create title |> get_or_failwith)
                (Description.create description
                 |> get_or_failwith
                 |> CCOption.return)
                (Url.create url |> get_or_failwith)
                (database |> Database.label)
                (title |> CCString.take 11 |> GtxSender.of_string)
                (Styles.Write.create styles
                 |> get_or_failwith
                 |> CCOption.return)
                (Icon.Write.create icon |> get_or_failwith |> CCOption.return)
                None
                (Pool_common.Language.create default_language |> get_or_failwith)
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
            [ Pool_tenant.Created (tenant, database)
            ; Pool_tenant.LogosUploaded logo_mappings
            ]
            |> Lwt_list.iter_s (Pool_tenant.handle_event Database.root))
  in
  Lwt.return_unit
;;
