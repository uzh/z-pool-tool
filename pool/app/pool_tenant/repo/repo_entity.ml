open Entity
module Common = Pool_common
module Database = Pool_database
module SmtpAuth = Repo_entity_smtp_auth

module Title = struct
  include Title

  let t = Caqti_type.string
end

module Description = struct
  include Description

  let t = Caqti_type.string
end

module Url = struct
  include Entity.Url

  let t = Caqti_type.string

  let find_url_request =
    {sql| SELECT url FROM pool_tenant WHERE database_label = ? |sql}
    |> Caqti_request.find Database.Repo.Label.t t
  ;;

  let of_pool pool =
    let open Lwt.Infix in
    Utils.Database.find_opt
      (Database.Label.value Database.root)
      find_url_request
      pool
    >|= function
    | None ->
      Sihl.Configuration.read_string "PUBLIC_URL"
      |> CCOption.get_exn_or "PUBLIC_URL not found in configuration"
    | Some url -> url
  ;;
end

module Styles = struct
  include Styles

  let t = Common.Repo.File.t

  module Write = struct
    include Write

    let t = Caqti_type.string
  end
end

module Icon = struct
  include Icon

  let t = Common.Repo.File.t

  module Write = struct
    include Write

    let t = Caqti_type.string
  end
end

module Maintenance = struct
  include Maintenance

  let t = Caqti_type.bool
end

module Disabled = struct
  include Disabled

  let t = Caqti_type.bool
end

let t =
  let open Entity.Read in
  let encode m =
    Ok
      ( Id.value m.Read.id
      , ( m.title
        , ( m.description
          , ( Url.value m.url
            , ( m.database_label
              , ( m.smtp_auth
                , ( m.styles
                  , ( m.icon
                    , ( m.maintenance
                      , ( m.disabled
                        , (m.default_language, (m.created_at, m.updated_at)) )
                      ) ) ) ) ) ) ) ) )
  in
  let decode
      ( id
      , ( title
        , ( description
          , ( url
            , ( database_label
              , ( smtp_auth
                , ( styles
                  , ( icon
                    , ( maintenance
                      , (disabled, (default_language, (created_at, updated_at)))
                      ) ) ) ) ) ) ) ) )
    =
    let open CCResult in
    map_err (fun _ -> "decode tenant read")
    @@ let* title = Title.create title in
       let* description = Description.create description in
       let* url = Url.create url in
       Ok
         { id = Id.of_string id
         ; title
         ; description
         ; url
         ; database_label
         ; smtp_auth
         ; styles
         ; icon
         ; maintenance = Maintenance.create maintenance
         ; disabled = Disabled.create disabled
         ; default_language
         ; created_at
         ; updated_at
         }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Common.Repo.Id.t
         (tup2
            Title.t
            (tup2
               Description.t
               (tup2
                  Url.t
                  (tup2
                     Database.Repo.Label.t
                     (tup2
                        SmtpAuth.t
                        (tup2
                           Styles.t
                           (tup2
                              Icon.t
                              (tup2
                                 Maintenance.t
                                 (tup2
                                    Disabled.t
                                    (tup2
                                       Pool_common.Language.t
                                       (tup2
                                          Common.Repo.CreatedAt.t
                                          Common.Repo.UpdatedAt.t)))))))))))))
;;

module Write = struct
  open Entity.Write

  let t =
    let encode m =
      Ok
        ( Id.value m.Write.id
        , ( m.title
          , ( m.description
            , ( Url.value m.url
              , ( m.database
                , ( m.smtp_auth
                  , ( m.styles
                    , ( m.icon
                      , ( m.maintenance
                        , ( m.disabled
                          , (m.default_language, (m.created_at, m.updated_at))
                          ) ) ) ) ) ) ) ) ) )
    in
    let decode
        ( id
        , ( title
          , ( description
            , ( url
              , ( database
                , ( smtp_auth
                  , ( styles
                    , ( icon
                      , ( maintenance
                        , ( disabled
                          , (default_language, (created_at, updated_at)) ) ) )
                    ) ) ) ) ) ) )
      =
      let open CCResult in
      map_err (fun _ -> "decode tenant write")
      @@ let* title = Title.create title in
         let* description = Description.create description in
         let* url = Url.create url in
         Ok
           { id = Id.of_string id
           ; title
           ; description
           ; url
           ; database
           ; smtp_auth
           ; styles
           ; icon
           ; maintenance = Maintenance.create maintenance
           ; disabled = Disabled.create disabled
           ; default_language
           ; created_at
           ; updated_at
           }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           Common.Repo.Id.t
           (tup2
              Title.t
              (tup2
                 Description.t
                 (tup2
                    Url.t
                    (tup2
                       Database.Repo.t
                       (tup2
                          SmtpAuth.Write.t
                          (tup2
                             Styles.Write.t
                             (tup2
                                Icon.Write.t
                                (tup2
                                   Maintenance.t
                                   (tup2
                                      Disabled.t
                                      (tup2
                                         Pool_common.Language.t
                                         (tup2
                                            Common.Repo.CreatedAt.t
                                            Common.Repo.UpdatedAt.t)))))))))))))
  ;;
end

module Selection = struct
  open Entity.Selection

  let t =
    let encode m = Ok (Url.value m.Selection.url, m.database_label) in
    let decode (url, database_label) =
      let open CCResult in
      map_err (fun _ -> "decode tenant selection")
      @@ let* url = Url.create url in
         Ok { url; database_label }
    in
    Caqti_type.(custom ~encode ~decode (tup2 Url.t Database.Repo.Label.t))
  ;;
end
