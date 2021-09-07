open Common.Repo
open Entity

module RecruitmentChannel = struct
  include RecruitmentChannel

  let t =
    Caqti_type.(
      custom
        ~encode:(fun m -> m |> to_string |> Result.ok)
        ~decode:of_string
        string)
  ;;
end

let participant_caqti =
  let status =
    let encode m = m |> Sihl_user.status_to_string |> Result.ok in
    let decode = Sihl_user.status_of_string in
    Caqti_type.(custom ~encode ~decode string)
  in
  let open Sihl.Contract.User in
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
    (* TODO checks for confirmed and not admin users only, a Person should just
       be valid, if it was confirmed. Check if there is a better place for
       this. *)
    match confirmed, admin with
    | false, _ -> Error "User is not confirmed"
    | _, true -> Error "An admin cannot be a participant"
    | _ ->
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

let t =
  let encode m =
    Ok
      ( m.user
      , ( m.recruitment_channel
        , ( m.terms_accepted_at
          , (m.paused, (m.disabled, (m.verified, (m.created_at, m.updated_at))))
          ) ) )
  in
  let decode
      ( user
      , ( recruitment_channel
        , ( terms_accepted_at
          , (paused, (disabled, (verified, (created_at, updated_at)))) ) ) )
    =
    Ok
      { user
      ; recruitment_channel
      ; terms_accepted_at
      ; paused
      ; disabled
      ; verified
      ; created_at
      ; updated_at
      }
  in
  Caqti_type.(
    custom
      ~encode
      ~decode
      (tup2
         participant_caqti
         (tup2
            RecruitmentChannel.t
            (tup2
               TermsAccepted.t
               (tup2
                  Paused.t
                  (tup2 Disabled.t (tup2 Verified.t (tup2 ptime ptime))))))))
;;

let find = Utils.todo
let insert = Utils.todo
let update _ = Utils.todo

let set_password : t -> string -> string -> (unit, string) result Lwt.t =
 fun { user; _ } password password_confirmation ->
  let open Lwt_result.Infix in
  Service.User.set_password user ~password ~password_confirmation
  >|= CCFun.const ()
;;
