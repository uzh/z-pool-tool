open Entity
module Common = Pool_common.Repo
open CCFun

let show_error = Pool_common.(Utils.error_to_string Language.En)

module Id = struct
  include Id

  let t =
    let encode = Utils.fcn_ok value in
    let decode = Utils.fcn_ok of_string in
    Caqti_type.(custom ~encode ~decode string)
  ;;
end

module Label = struct
  let t =
    Caqti_type.(
      custom
        ~encode:(Label.show %> CCResult.pure)
        ~decode:(Label.of_string %> CCResult.map_err show_error)
        string)
  ;;
end

module EmailSubject = struct
  include EmailSubject

  let t =
    Caqti_type.(
      custom
        ~encode:(value %> CCResult.pure)
        ~decode:(create %> CCResult.map_err show_error)
        string)
  ;;
end

module EmailText = struct
  include EmailText

  let t =
    Caqti_type.(
      custom
        ~encode:(value %> CCResult.pure)
        ~decode:(create %> CCResult.map_err show_error)
        string)
  ;;
end

module PlainText = struct
  include PlainText

  let t =
    Caqti_type.(
      custom
        ~encode:(value %> CCResult.pure)
        ~decode:(create %> CCResult.map_err show_error)
        string)
  ;;
end

module SmsText = struct
  include SmsText

  let t =
    Caqti_type.(
      custom
        ~encode:(value %> CCResult.pure)
        ~decode:(create %> CCResult.map_err show_error)
        string)
  ;;
end

let t =
  let encode (m : t) =
    Ok
      ( m.id
      , ( m.label
        , ( m.entity_uuid
          , ( m.language
            , (m.email_subject, (m.email_text, (m.plain_text, m.sms_text))) ) )
        ) )
  in
  let decode
    ( id
    , ( label
      , ( entity_uuid
        , (language, (email_subject, (email_text, (plain_text, sms_text)))) ) )
    )
    =
    Ok
      { id
      ; label
      ; entity_uuid
      ; language
      ; email_subject
      ; email_text
      ; plain_text
      ; sms_text
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         Id.t
         (tup2
            Label.t
            (tup2
               (option Common.Id.t)
               (tup2
                  Common.Language.t
                  (tup2
                     EmailSubject.t
                     (tup2 EmailText.t (tup2 PlainText.t SmsText.t))))))))
;;
