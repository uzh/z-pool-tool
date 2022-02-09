include
  Ocaml_authorize.Role_s
    with type t =
      [ `Admin
      | `Tenant
      | `Guest
      | `Student
      | `Experiment
      | `System
      | `User
      | `Participant
      | `Contact
      | `Waiting_list
      | `Assignment
      | `Mailing
      | `Assistant of Ocaml_authorize.Uuidm.t
      | `Experimenter of Ocaml_authorize.Uuidm.t
      | `Location_manager of Ocaml_authorize.Uuidm.t
      | `Operator of Ocaml_authorize.Uuidm.t
      | `Recruiter of Ocaml_authorize.Uuidm.t
      ]
