open CCFun
open Ppx_yojson_conv_lib.Yojson_conv
module JobName = Pool_common.Repo.Model.SelectorType (Entity.JobName)
module Status = Pool_common.Repo.Model.SelectorType (Entity.Status)

module Id = struct
  include Entity.Id

  let t =
    Database.Repo.make_caqti_type
      Caqti_type.string
      (of_string %> CCResult.return)
      value
  ;;
end

module Instance = struct
  include Entity.Instance

  type ctx = (string * string) list [@@deriving yojson]

  let ctx =
    let encode = yojson_of_ctx %> Yojson.Safe.to_string %> CCResult.return in
    let decode m =
      try Yojson.Safe.from_string m |> ctx_of_yojson |> CCResult.return with
      | _ -> Error (Format.sprintf "failed to decode ctx %s" m)
    in
    Caqti_type.(custom ~encode ~decode string)
  ;;

  let t =
    let open Database.Caqti_encoders in
    let decode
      ( id
      , ( name
        , ( input
          , ( tries
            , ( next_run_at
              , ( max_tries
                , (status, (last_error, (last_error_at, (tag, (ctx, ()))))) ) )
            ) ) ) )
      =
      Ok
        { id
        ; name
        ; input
        ; tries
        ; next_run_at
        ; max_tries
        ; status
        ; last_error
        ; last_error_at
        ; tag
        ; ctx
        }
    in
    let encode (m : t) : ('a Data.t, string) result =
      Ok
        Data.
          [ m.id
          ; m.name
          ; m.input
          ; m.tries
          ; m.next_run_at
          ; m.max_tries
          ; m.status
          ; m.last_error
          ; m.last_error_at
          ; m.tag
          ; m.ctx
          ]
    in
    let open Schema in
    custom
      ~encode
      ~decode
      Caqti_type.
        [ Id.t
        ; JobName.t
        ; string
        ; int
        ; ptime
        ; int
        ; Status.t
        ; option string
        ; option ptime
        ; option string
        ; ctx
        ]
  ;;
end

module History = struct
  open Entity_history

  let write =
    let encode m =
      Ok (m.job.Entity.Instance.id, m.entity_uuid, m.message_template)
    in
    let decode _ =
      Pool_message.Error.WriteOnlyModel |> Pool_common.Utils.failwith
    in
    Caqti_type.(
      custom ~encode ~decode (t3 Id.t Pool_common.Repo.Id.t (option string)))
  ;;

  let read =
    let open Pool_common.Repo in
    let encode (_ : Entity_history.t) =
      Pool_message.Error.ReadOnlyModel |> Pool_common.Utils.failwith
    in
    let decode (entity_uuid, message_template, job) =
      Ok { Entity_history.entity_uuid; message_template; job }
    in
    Caqti_type.(custom ~encode ~decode (t3 Id.t (option string) Instance.t))
  ;;
end
