open Entity
module Common = Pool_common
module Id = Common.Id
module RepoId = Common.Repo.Id

module Public = struct
  let t =
    let encode (m : public) =
      Ok (Id.value m.id, Experiment.Description.value m.description)
    in
    let decode (id, description) =
      let open CCResult in
      map_err (fun _ ->
          Common.(
            Utils.error_to_string
              Language.En
              (Message.Decode Message.Field.I18n)))
      @@ let* description = Experiment.Description.create description in
         (* TODO[timhub]: Add sessions *)
         Ok { id = Id.of_string id; description; sessions = [] }
    in
    Caqti_type.(
      custom ~encode ~decode (tup2 RepoId.t Experiment.Repo.Description.t))
  ;;
end
