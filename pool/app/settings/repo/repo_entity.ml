open Entity

module TenantLanguages = struct
  include TenantLanguages

  let t =
    let encode m =
      Ok (m.values |> Values.to_string, (m.created_at, m.updated_at))
    in
    let decode (values, (created_at, updated_at)) =
      let open CCResult in
      let* values = values |> Values.of_string in
      Ok { values; created_at; updated_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           string
           (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))
  ;;

  module Values = struct
    include Values

    let t =
      Caqti_type.(
        custom ~encode:(fun m -> Ok (to_string m)) ~decode:of_string string)
    ;;
  end
end

module TenantEmailSuffixes = struct
  include TenantEmailSuffixes

  let t =
    let encode m =
      Ok (m.values |> Values.to_string, (m.created_at, m.updated_at))
    in
    let decode (values, (created_at, updated_at)) =
      let open CCResult in
      let* values = values |> Values.of_string in
      Ok { values; created_at; updated_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           string
           (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))
  ;;

  module Values = struct
    include Values

    let t =
      Caqti_type.(
        custom ~encode:(fun m -> Ok (to_string m)) ~decode:of_string string)
    ;;
  end
end

module TenantContactEmail = struct
  include TenantContactEmail

  let t =
    let encode m = Ok (m.value, (m.created_at, m.updated_at)) in
    let decode (value, (created_at, updated_at)) =
      let open CCResult in
      let* value = value |> ContactEmail.create in
      Ok { value; created_at; updated_at }
    in
    Caqti_type.(
      custom
        ~encode
        ~decode
        (tup2
           string
           (tup2 Pool_common.Repo.CreatedAt.t Pool_common.Repo.UpdatedAt.t)))
  ;;
end
