open Entity

let t =
  let encode m =
    Ok
      ( m.id
      , ( m.title
        , ( m.description
          , ( m.url
            , ( m.database
              , ( m.styles
                , ( m.icon
                  , ( m.logos
                    , ( m.partner_logos
                      , ( m.disabled
                        , (m.default_language, (m.created_at, m.updated_at)) )
                      ) ) ) ) ) ) ) ) )
  in
  let decode
      ( id
      , ( title
        , ( description
          , ( url
            , ( database
              , ( styles
                , ( icon
                  , ( logos
                    , ( partner_logos
                      , (disabled, (default_language, (created_at, updated_at)))
                      ) ) ) ) ) ) ) ) )
    =
    Ok
      { id
      ; title
      ; description
      ; url
      ; database
      ; styles
      ; icon
      ; logos
      ; partner_logos
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
      (tup2
         Id.t
         (tup2
            Title.t
            (tup2
               Description.t
               (tup2
                  Url.t
                  (tup2
                     Database.t
                     (tup2
                        Styles.t
                        (tup2
                           Icon.t
                           (tup2
                              Logos.t
                              (tup2
                                 PartnerLogo.t
                                 (tup2
                                    Disabled.t
                                    (tup2
                                       Settings.Language.t
                                       (tup2 ptime ptime)))))))))))))
;;

let find_by_id (id : string) : (t, string) result Lwt.t = Utils.todo id
let list_all = Utils.todo
let insert (t : t) = Utils.todo t
let update (t : t) = Utils.todo t
let destroy = Utils.todo
