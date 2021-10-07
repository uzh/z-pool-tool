open Entity

let encode_person
    : type a.
      [< `Assistant
      | `Experimenter
      | `Recruiter
      | `LocationManager
      | `Operator
      | `Root
      ]
      -> a Entity.t
      -> (string * (Sihl_user.t * (Ptime.t * Ptime.t)), 'b) result
  =
 fun carrier person ->
  match person with
  | Assistant m | Experimenter m | LocationManager m | Recruiter m | Operator m
    ->
    let role = carrier |> Utils.Stringify.person in
    Ok (role, (m.user, (m.created_at, m.updated_at)))
;;

let decode_person constructor (_, (user, (created_at, updated_at))) =
  Ok (constructor { user; created_at; updated_at })
;;

let make_caqti_type person decode_fcn =
  Caqti_type.(
    custom
      ~encode:(encode_person person)
      ~decode:(decode_person decode_fcn)
      (tup2 string (tup2 Common_user.Repo.user_caqti (tup2 ptime ptime))))
;;

let assistant = make_caqti_type `Assistant @@ fun person -> Assistant person

let experimenter =
  make_caqti_type `Experimenter @@ fun person -> Experimenter person
;;

let location_manager =
  make_caqti_type `LocationManager @@ fun person -> LocationManager person
;;

let recruiter = make_caqti_type `Recruiter @@ fun person -> Recruiter person
let operator = make_caqti_type `Operator @@ fun person -> Operator person

module Write = struct
  type t =
    { role : string
    ; sihl_user_id : string
    ; created_at : Pool_common.CreatedAt.t
    ; updated_at : Pool_common.UpdatedAt.t
    }

  let extract : type person. person Entity.t -> t =
    let values person role =
      { role
      ; sihl_user_id = person.user.Sihl.Contract.User.id
      ; created_at = person.created_at
      ; updated_at = person.updated_at
      }
    in
    function
    | Assistant person -> values person (Utils.Stringify.person `Assistant)
    | Experimenter person ->
      values person (Utils.Stringify.person `Experimenter)
    | LocationManager person ->
      values person (Utils.Stringify.person `LocationManager)
    | Recruiter person -> values person (Utils.Stringify.person `Recruiter)
    | Operator person -> values person (Utils.Stringify.person `Operator)
  ;;

  let caqti =
    let encode m =
      Ok (m.role, (m.sihl_user_id, (m.created_at, m.updated_at)))
    in
    let decode _ = Error "Model only used for DB insert" in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           string
           (tup2
              string
              (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t))))
  ;;
end
