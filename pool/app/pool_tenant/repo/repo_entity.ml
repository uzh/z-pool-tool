open CCFun
open Entity

module Id = struct
  include Id

  let t =
    Pool_common.Repo.make_caqti_type
      Caqti_type.string
      (of_string %> CCResult.return)
      value
  ;;
end

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
    Database.find_opt Database.Pool.Root.label find_url_request pool
    ||> function
    | None ->
      Sihl.Configuration.read_string "PUBLIC_URL"
      |> CCOption.get_exn_or "PUBLIC_URL not found in configuration"
      |> Url.of_string
    | Some url -> url
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

module EmailLogo = struct
  include EmailLogo

  let t = Common.Repo.File.t

  module Write = struct
    include Write

    let t = Pool_common.Repo.make_caqti_type Caqti_type.string create value
  end
end

let t =
  let open Entity.Read in
  let open Caqti_type in
  let open Database.Caqti_encoders in
  let encode m : ('a Data.t, string) result =
    Ok
      Data.
        [ m.Read.id
        ; m.title
        ; m.description
        ; m.url
        ; m.default_language
        ; m.created_at
        ; m.updated_at
        ; m.status
        ; m.database_label
        ; m.styles
        ; m.email_logo
        ; m.icon
        ]
  in
  let decode
        ( id
        , ( title
          , ( description
            , ( url
              , ( default_language
                , ( created_at
                  , ( updated_at
                    , (status, (database_label, (styles, (icon, (email_logo, ()))))) ) )
                ) ) ) ) )
    =
    Ok
      { id
      ; title
      ; description
      ; url
      ; database_label
      ; styles
      ; icon
      ; status
      ; email_logo
      ; default_language
      ; created_at
      ; updated_at
      }
  in
  custom
    ~encode
    ~decode
    Schema.
      [ Pool_common.Repo.Id.t
      ; Title.t
      ; option Description.t
      ; Url.t
      ; Pool_common.Repo.Language.t
      ; Pool_common.Repo.CreatedAt.t
      ; Pool_common.Repo.UpdatedAt.t
      ; Database.Repo.Status.t
      ; Database.Repo.Label.t
      ; option Styles.t
      ; option Icon.t
      ; option EmailLogo.t
      ]
;;

module Write = struct
  open Entity.Write

  let t =
    let open Caqti_type in
    let open Database.Caqti_encoders in
    let encode m : ('a Data.t, string) result =
      Ok
        Data.
          [ m.Write.id
          ; m.title
          ; m.description
          ; m.url
          ; m.default_language
          ; m.created_at
          ; m.updated_at
          ; m.database_label
          ; m.styles
          ; m.icon
          ; m.email_logo
          ]
    in
    let decode
          ( id
          , ( title
            , ( description
              , ( url
                , ( default_language
                  , ( created_at
                    , (updated_at, (database_label, (styles, (icon, (email_logo, ())))))
                    ) ) ) ) ) )
      =
      Ok
        { id
        ; title
        ; description
        ; url
        ; database_label
        ; styles
        ; icon
        ; email_logo
        ; default_language
        ; created_at
        ; updated_at
        }
    in
    custom
      ~encode
      ~decode
      Schema.
        [ Pool_common.Repo.Id.t
        ; Title.t
        ; option Description.t
        ; Url.t
        ; Pool_common.Repo.Language.t
        ; Pool_common.Repo.CreatedAt.t
        ; Pool_common.Repo.UpdatedAt.t
        ; Database.Repo.Label.t
        ; option Styles.Write.t
        ; option Icon.Write.t
        ; option EmailLogo.Write.t
        ]
  ;;
end
