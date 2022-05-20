module Error = struct
  type error = Entity_message.error [@@deriving eq, show, yojson]

  let invalid_bool = Entity_message.(ConformistModuleErrorType)
  let invalid_float = Entity_message.(ConformistModuleErrorType)
  let invalid_int = Entity_message.(ConformistModuleErrorType)
  let invalid_string = Entity_message.(ConformistModuleErrorType)
  let invalid_date = Entity_message.(ConformistModuleErrorType)
  let invalid_datetime = Entity_message.(ConformistModuleErrorType)
  let no_value = Entity_message.(NoValue)
  let of_string _ = Entity_message.(ConformistModuleErrorType)
end

module PoolConformist = Conformist.Make (Error)

let with_log_info ?(level = Logs.Info) info =
  Logs.msg level (fun m -> m "%s" (Locales_en.info_to_string info));
  info
;;

let with_log_success ?(level = Logs.Info) success =
  Logs.msg level (fun m -> m "%s" (Locales_en.success_to_string success));
  success
;;

let with_log_warning ?(level = Logs.Warning) warn =
  Logs.msg level (fun m -> m "%s" (Locales_en.warning_to_string warn));
  warn
;;

let with_log_error ?(level = Logs.Error) err =
  Logs.msg level (fun m -> m "%s" (Locales_en.error_to_string err));
  err
;;

let with_log_result_error fcn =
  CCResult.map_err (fun err ->
      let _ = err |> fcn |> with_log_error in
      err)
;;

let decoder create_fcn field l =
  let open CCResult in
  match l with
  | x :: _ -> create_fcn x
  | [] ->
    Error (Entity_message.Undefined field |> with_log_error ~level:Logs.Info)
;;

let schema_decoder create_fcn encode_fnc field =
  PoolConformist.custom
    (decoder create_fcn field)
    (fun l -> l |> encode_fnc |> CCList.pure)
    Entity_message.Field.(field |> show)
;;

let list_decoder create_fcn l = create_fcn l

let schema_list_decoder create_fcn encode_fnc field =
  PoolConformist.custom
    (list_decoder create_fcn)
    (fun l -> l |> encode_fnc)
    Entity_message.Field.(field |> show)
;;

let parse_time str =
  let open CCResult in
  Ptime.of_rfc3339 str
  |> Ptime.rfc3339_error_to_msg
  |> CCResult.map_err (fun (`Msg e) -> Entity_message.NotADatetime (str, e))
  (* TODO [aerben] dont discard timezone *)
  (* TODO [aerben] should experimenter add timezone for start? *)
  (* TODO [aerben] HANDLE ALL TIMEZONES consistently *)
  >|= fun (time, _, _) -> time
;;

let parse_time_span str =
  if CCString.is_empty str
  then Error Entity_message.NoValue
  else
    str
    |> CCInt.of_string
    |> CCOption.to_result Entity_message.(Invalid Field.Duration)
    |> CCResult.map Ptime.Span.of_int_s
;;

let print_time_span span =
  Ptime.Span.to_int_s span
  |> CCOption.map CCInt.to_string
  (* If span is bigger than int max (signed), print an info string instead *)
  (* This is only the case if span > 136 years (64bit system) *)
  |> CCOption.get_or ~default:"Session duration too long!"
;;

module type BaseSig = sig
  type t

  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val create : t -> (t, Entity_message.error) result
  val schema : unit -> (Entity_message.error, t) PoolConformist.Field.t
end
