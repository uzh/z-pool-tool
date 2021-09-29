open Entity

let user_caqti =
  let open Sihl.Contract.User in
  let status =
    let encode m = m |> Sihl_user.status_to_string |> Result.ok in
    let decode = Sihl_user.status_of_string in
    Caqti_type.(custom ~encode ~decode string)
  in
  let encode m =
    Ok
      ( m.id
      , ( m.email
        , ( m.username
          , ( m.name
            , ( m.given_name
              , ( m.password
                , ( m.status
                  , (m.admin, (m.confirmed, (m.created_at, m.updated_at))) ) )
              ) ) ) ) )
  in
  let decode
      ( id
      , ( email
        , ( username
          , ( name
            , ( given_name
              , ( password
                , (status, (admin, (confirmed, (created_at, updated_at)))) ) )
            ) ) ) )
    =
    (* TODO checks for confirmed users only, a Person should just be valid, if
       it was confirmed. Check if there is a better place for this. *)
    if not confirmed
    then Error "User is not confirmed"
    else
      Ok
        { id
        ; email
        ; username
        ; name
        ; given_name
        ; password
        ; status
        ; admin
        ; confirmed
        ; created_at
        ; updated_at
        }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         string
         (tup2
            string
            (tup2
               (option string)
               (tup2
                  (option string)
                  (tup2
                     (option string)
                     (tup2
                        string
                        (tup2 status (tup2 bool (tup2 bool (tup2 ptime ptime)))))))))))
;;

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
  | Assistant m
  | Experimenter m
  | LocationManager m
  | Recruiter m
  | Operator m
  | Root m ->
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
      (tup2 string (tup2 user_caqti (tup2 ptime ptime))))
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
let root = make_caqti_type `Root @@ fun person -> Root person
