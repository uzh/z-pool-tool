module MailingCommand = Cqrs_command.Mailing_command
module Conformist = Pool_common.Utils.PoolConformist
module Field = Pool_common.Message.Field
module Model = Test_utils.Model

let get_or_failwith = Pool_common.Utils.get_or_failwith

module Data = struct
  let norm_ptime m =
    m
    |> Ptime.to_rfc3339
    |> Pool_common.Utils.Time.parse_time
    |> get_or_failwith
  ;;

  module Mailing = struct
    let id = Mailing.Id.create ()

    let start_at =
      Ptime_clock.now ()
      |> CCFun.flip Ptime.add_span (Ptime.Span.of_int_s 1800)
      |> CCOption.get_exn_or "Mailing Test: Could not create end_at timestamp."
      |> norm_ptime
      |> Mailing.StartAt.create
      |> get_or_failwith
    ;;

    let end_at =
      Ptime_clock.now ()
      |> CCFun.flip Ptime.add_span (Ptime.Span.of_int_s 3600)
      |> CCOption.get_exn_or "Mailing Test: Could not create end_at timestamp."
      |> norm_ptime
      |> Mailing.EndAt.create
      |> get_or_failwith
    ;;

    let rate = Mailing.Rate.create 200 |> get_or_failwith

    let distribution =
      Mailing.Distribution.
        [ SortableField.Lastname, SortOrder.Ascending
        ; SortableField.InvitationCount, SortOrder.Descending
        ]
    ;;

    let create =
      let open Mailing in
      [ Field.(Start |> show), [ start_at |> StartAt.value |> Ptime.to_rfc3339 ]
      ; Field.(End |> show), [ end_at |> EndAt.value |> Ptime.to_rfc3339 ]
      ; Field.(Rate |> show), [ rate |> Rate.value |> CCInt.to_string ]
      ; ( Field.(Distribution |> show)
        , [ distribution
            |> Distribution.yojson_of_sorted
            |> Yojson.Safe.to_string
          ] )
      ]
    ;;

    let create_end_before_start =
      let open Mailing in
      [ Field.(Start |> show), [ end_at |> EndAt.value |> Ptime.to_rfc3339 ]
      ; Field.(End |> show), [ start_at |> StartAt.value |> Ptime.to_rfc3339 ]
      ; Field.(Rate |> show), [ rate |> Rate.value |> CCInt.to_string ]
      ; ( Field.(Distribution |> show)
        , [ distribution
            |> Distribution.yojson_of_sorted
            |> Yojson.Safe.to_string
          ] )
      ]
    ;;
  end
end

let create_mailing () =
  let open Data.Mailing in
  Mailing.
    { id
    ; start_at
    ; end_at
    ; rate
    ; distribution = Some (distribution |> Distribution.create_sorted)
    ; created_at = Pool_common.CreatedAt.create ()
    ; updated_at = Pool_common.UpdatedAt.create ()
    }
;;

let create () =
  let open MailingCommand.Create in
  let experiment = Model.create_experiment () in
  let mailing = create_mailing () in
  let events =
    Data.Mailing.create
    |> Http_utils.format_request_boolean_values Field.[ RandomOrder |> show ]
    |> Http_utils.remove_empty_values
    |> decode
    |> get_or_failwith
    |> handle ~id:Data.Mailing.id experiment
  in
  let expected =
    Ok
      [ Mailing.Created (mailing, experiment.Experiment.id)
        |> Pool_event.mailing
      ]
  in
  Test_utils.check_result expected events
;;

let create_with_distribution () =
  let open Mailing in
  let open CCResult in
  let open MailingCommand.Create in
  let open Mailing.Distribution in
  let mailing = create_mailing () in
  let distribution =
    SortableField.
      [ InvitationCount, SortOrder.Ascending
      ; AssignmentCount, SortOrder.Descending
      ]
  in
  let mailing =
    { mailing with distribution = Some (distribution |> create_sorted) }
  in
  let experiment = Model.create_experiment () in
  let urlencoded () =
    distribution
    |> CCList.map (fun (field, sort) ->
         Format.asprintf
           "%s,%s"
           (SortableField.show field)
           (SortOrder.show sort))
    |> of_urlencoded_list
    >|= fun distribution ->
    let show = Field.show in
    [ show Field.Start, mailing.start_at |> StartAt.value |> Ptime.to_rfc3339
    ; show Field.End, mailing.end_at |> EndAt.value |> Ptime.to_rfc3339
    ; show Field.Rate, mailing.rate |> Rate.value |> CCInt.to_string
    ; show Field.Distribution, distribution
    ]
    |> CCList.map (fun (field, value) -> field, [ value ])
    |> Http_utils.format_request_boolean_values Field.[ RandomOrder |> show ]
  in
  (* let () = let encoded = urlencoded () |> Pool_common.Utils.get_or_failwith
     in raise (Failure ([%show: (string * string list) list] encoded)) in *)
  let events =
    () |> urlencoded >>= decode >>= handle ~id:Data.Mailing.id experiment
  in
  let expected =
    Ok
      [ Mailing.Created (mailing, experiment.Experiment.id)
        |> Pool_event.mailing
      ]
  in
  Test_utils.check_result expected events
;;

let create_end_before_start () =
  let open MailingCommand.Create in
  let experiment = Model.create_experiment () in
  let events =
    Data.Mailing.create_end_before_start
    |> Http_utils.format_request_boolean_values Field.[ RandomOrder |> show ]
    |> Http_utils.remove_empty_values
    |> decode
    |> get_or_failwith
    |> handle ~id:Data.Mailing.id experiment
  in
  let expected = Error Pool_common.Message.EndBeforeStart in
  Test_utils.check_result expected events
;;
