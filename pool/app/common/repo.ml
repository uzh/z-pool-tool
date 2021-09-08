open Entity

module Id = struct
  include Id

  let t = Caqti_type.string
end

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

  let insert = Utils.todo
  let update = Utils.todo
end
