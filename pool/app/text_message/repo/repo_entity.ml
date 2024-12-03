open Entity

let delivery_report =
  let open Caqti_type in
  let open Database.Caqti_encoders in
  let encode m : ('a Data.t, string) result =
    Ok
      Data.
        [ m.job_id
        ; m.raw
        ; m.from
        ; m.to_
        ; m.message_id
        ; m.dlr_mask
        ; m.error_code
        ; m.error_message
        ; m.submit_date
        ; m.done_date
        ; m.plmn
        ; m.country
        ; m.sms_cost
        ]
  in
  let decode
        ( job_id
        , ( raw
          , ( from
            , ( to_
              , ( message_id
                , ( dlr_mask
                  , ( error_code
                    , ( error_message
                      , (submit_date, (done_date, (plmn, (country, (sms_cost, ()))))) ) )
                  ) ) ) ) ) )
    =
    Ok
      { job_id
      ; raw
      ; from
      ; to_
      ; message_id
      ; dlr_mask
      ; error_code
      ; error_message
      ; submit_date
      ; done_date
      ; plmn
      ; country
      ; sms_cost
      }
  in
  custom
    ~encode
    ~decode
    Schema.
      [ Pool_queue.Repo.Id.t
      ; string
      ; string
      ; string
      ; string
      ; int
      ; int
      ; string
      ; ptime
      ; ptime
      ; string
      ; string
      ; float
      ]
;;
