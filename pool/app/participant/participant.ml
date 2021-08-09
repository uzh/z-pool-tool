include Event
include Entity

let login _ ~email:_ ~password:_ = Sihl.todo ()
let find_by_user = Sihl.todo

let recruitment_channel_of_string =
  let open RecruitmentChannel in
  function
  | "friend" -> Ok Friend
  | "online" -> Ok Online
  | "lecture" -> Ok Lecture
  | "mailing" -> Ok Mailing
  | _ -> Error "Invalid recruitment channel provided"
;;

let recruitment_channel_to_string =
  let open RecruitmentChannel in
  function
  | Friend -> "friend"
  | Online -> "online"
  | Lecture -> "lecture"
  | Mailing -> "mailing"
;;

let strip_email_suffix email =
  (* TODO check whether this is stable *)
  let tail = String.split_on_char '@' email |> CCList.tail_opt in
  Option.bind tail CCList.head_opt
;;

let validate_email allowed_email_suffixes email =
  match allowed_email_suffixes with
  | None -> Ok ()
  | Some allowed_email_suffixes ->
    let suffix = strip_email_suffix email in
    (match suffix with
    (* TODO check whether this is really the case *)
    | None -> Error "Email malformed"
    | Some suffix ->
      if CCList.mem ~eq:String.equal suffix allowed_email_suffixes
      then Ok ()
      else Error "Invalid email suffix provided")
;;
