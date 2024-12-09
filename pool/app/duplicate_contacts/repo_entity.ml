open Entity

let make_caqti_type = Pool_common.Repo.make_caqti_type

module Id = struct
  include Id

  let t = make_caqti_type Caqti_type.string CCFun.(of_string %> CCResult.return) value
end

let t =
  let open Database.Caqti_encoders in
  let open CCResult in
  let decode (id, (contact_a, (contact_b, (score, (ignored, ()))))) =
    Ok { id; contact_a; contact_b; score; ignored }
  in
  let encode _ = Pool_common.Utils.failwith Pool_message.Error.ReadOnlyModel in
  let open Schema in
  custom ~encode ~decode Caqti_type.[ Id.t; Contact.Repo.t; Contact.Repo.t; float; bool ]
;;

let raw = Caqti_type.(t3 Pool_common.Repo.Id.t Pool_common.Repo.Id.t float)
