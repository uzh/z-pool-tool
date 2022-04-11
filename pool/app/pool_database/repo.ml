include Entity

module Label = struct
  include Label

  let t =
    let open CCResult in
    Caqti_type.(
      custom
        ~encode:pure
        ~decode:(fun m ->
          map_err (fun _ ->
              let open Pool_common in
              Utils.error_to_string
                Language.En
                Message.(Decode Field.DatabaseLabel))
          @@ create m)
        string)
  ;;
end

module Url = struct
  include Url

  let t = Caqti_type.string
end

let t =
  let open CCResult in
  let encode m = Ok (m.url, m.label) in
  let decode (url, label) =
    map_err (fun _ ->
        let open Pool_common in
        Utils.error_to_string Language.En Message.(Decode Field.Database))
    @@ let* url = Url.create url in
       let* label = Label.create label in
       Ok { url; label }
  in
  Caqti_type.(custom ~encode ~decode (tup2 Url.t Label.t))
;;
