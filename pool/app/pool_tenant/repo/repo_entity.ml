open CCFun
open Entity
module Common = Pool_common
module Database = Pool_database

module Title = struct
  include Title

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Description = struct
  include Description

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
end

module Url = struct
  include Entity.Url

  let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value

  let find_url_request =
    let open Caqti_request.Infix in
    {sql| SELECT url FROM pool_tenant WHERE database_label = ? |sql}
    |> Database.Repo.Label.t ->! t
  ;;

  let of_pool pool =
    let open Utils.Lwt_result.Infix in
    Utils.Database.find_opt
      (Database.Label.value Database.root)
      find_url_request
      pool
    ||> function
    | None ->
      Sihl.Configuration.read_string "PUBLIC_URL"
      |> CCOption.get_exn_or "PUBLIC_URL not found in configuration"
    | Some url -> url
  ;;
end

module GtxApiKey = struct
  include GtxApiKey

  let t =
    let open Utils.Crypto.String in
    Common.Repo.make_caqti_type
      Caqti_type.string
      (decrypt_from_string
       %> CCResult.map_err (fun _ -> Common.Message.(Decode Field.GtxApiKey)))
      encrypt_to_string
  ;;
end

module Styles = struct
  include Styles

  let t = Common.Repo.File.t

  module Write = struct
    include Write

    let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
  end
end

module Icon = struct
  include Icon

  let t = Common.Repo.File.t

  module Write = struct
    include Write

    let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
  end
end

module Maintenance = struct
  include Maintenance

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

module Disabled = struct
  include Disabled

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.bool
      (create %> CCResult.return)
      value
  ;;
end

let t =
  let open Entity.Read in
  let encode m =
    Ok
      ( m.Read.id
      , ( m.title
        , ( m.description
          , ( Url.value m.url
            , ( m.database_label
              , ( m.styles
                , ( m.icon
                  , ( m.maintenance
                    , ( m.disabled
                      , (m.default_language, (m.created_at, m.updated_at)) ) )
                  ) ) ) ) ) ) )
  in
  let decode
    ( id
    , ( title
      , ( description
        , ( url
          , ( database_label
            , ( styles
              , ( icon
                , ( maintenance
                  , (disabled, (default_language, (created_at, updated_at))) )
                ) ) ) ) ) ) )
    =
    Ok
      { id
      ; title
      ; description
      ; url
      ; database_label
      ; styles
      ; icon
      ; maintenance
      ; disabled
      ; default_language
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (t2
         Common.Repo.Id.t
         (t2
            Title.t
            (t2
               (option Description.t)
               (t2
                  Url.t
                  (t2
                     Database.Repo.Label.t
                     (t2
                        (option Styles.t)
                        (t2
                           (option Icon.t)
                           (t2
                              Maintenance.t
                              (t2
                                 Disabled.t
                                 (t2
                                    Pool_common.Repo.Language.t
                                    (t2
                                       Common.Repo.CreatedAt.t
                                       Common.Repo.UpdatedAt.t))))))))))))
;;

module Write = struct
  open Entity.Write

  let t =
    let encode m =
      Ok
        ( m.Write.id
        , ( m.title
          , ( m.description
            , ( Url.value m.url
              , ( m.database
                , ( m.gtx_api_key
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
              , ( gtx_api_key
                , ( styles
                  , ( icon
                    , ( maintenance
                      , (disabled, (default_language, (created_at, updated_at)))
                      ) ) ) ) ) ) ) ) )
      =
      Ok
        { id
        ; title
        ; description
        ; url
        ; database
        ; gtx_api_key
        ; styles
        ; icon
        ; maintenance
        ; disabled
        ; default_language
        ; created_at
        ; updated_at
        }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (t2
           Common.Repo.Id.t
           (t2
              Title.t
              (t2
                 (option Description.t)
                 (t2
                    Url.t
                    (t2
                       Database.Repo.t
                       (t2
                          (option GtxApiKey.t)
                          (t2
                             (option Styles.Write.t)
                             (t2
                                (option Icon.Write.t)
                                (t2
                                   Maintenance.t
                                   (t2
                                      Disabled.t
                                      (t2
                                         Pool_common.Repo.Language.t
                                         (t2
                                            Common.Repo.CreatedAt.t
                                            Common.Repo.UpdatedAt.t)))))))))))))
  ;;
end

module Selection = struct
  open Entity.Selection

  let t =
    let encode m = Ok (m.Selection.url, m.database_label) in
    let decode (url, database_label) = Ok { url; database_label } in
    Caqti_type.(custom ~encode ~decode (t2 Url.t Database.Repo.Label.t))
  ;;
end
