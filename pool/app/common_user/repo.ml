open Entity

module Paused = struct
  include Paused

  let t = Caqti_type.bool
end

module Disabled = struct
  include Disabled

  let t = Caqti_type.bool
end

module TermsAccepted = struct
  include TermsAccepted

  let t = Caqti_type.ptime
end

module Verified = struct
  include Verified

  let t = Caqti_type.ptime
end

module Email = struct
  open Email

  let unverified_t =
    let encode (Unverified m) = Ok (m.address, m.token) in
    let decode (address, token) = Ok (Unverified { address; token }) in
    Caqti_type.(custom ~encode ~decode (tup2 string string))
  ;;

  let verified_t =
    let encode (Verified m) = Ok (m.address, m.verified_at) in
    let decode (address, verified_at) =
      Ok (Verified { address; verified_at })
    in
    Caqti_type.(custom ~encode ~decode (tup2 string ptime))
  ;;

  let insert _ = Utils.todo
  let update _ = Utils.todo
end

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
