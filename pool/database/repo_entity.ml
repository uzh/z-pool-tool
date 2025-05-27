open CCFun
open Entity

let make_caqti_type caqti_type create value =
  let encode = value %> CCResult.return in
  let decode = create %> CCResult.map_err Pool_message.Error.show in
  Caqti_type.(custom ~encode ~decode caqti_type)
;;

module Label = struct
  include Label

  let t = make_caqti_type Caqti_type.string create value
end

module Url = struct
  include Url

  let t = make_caqti_type Caqti_type.string decrypt encrypt
end

module Status = struct
  include Status

  let in_fragment =
    CCList.map (Status.show %> Format.asprintf "'%s'")
    %> CCString.concat ", "
    %> Format.asprintf "(%s)"
  ;;

  let t = make_caqti_type Caqti_type.string of_string show
end

let t =
  let decode (label, (url, (status, ()))) = Ok (create ~status label url) in
  let encode t : ('a Caqti_encoders.Data.t, string) result =
    Ok Caqti_encoders.Data.[ label t; url t; status t ]
  in
  Caqti_encoders.(custom ~encode ~decode Schema.[ Label.t; Url.t; Status.t ])
;;
