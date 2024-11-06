module ContactRepo = Integration_utils.ContactRepo

let pool = Test_utils.Data.database_label
let get_exn = Test_utils.get_or_failwith
let testable_score = Alcotest.(testable CCFloat.pp CCFloat.equal)

let check_similarity _ () =
  let open Utils.Lwt_result.Infix in
  let open Duplicate_contacts in
  let open Pool_user in
  let create_contact ~firstname ~lastname =
    ContactRepo.create
      ~firstname:(Firstname.of_string firstname)
      ~lastname:(Lastname.of_string lastname)
      ()
  in
  let find_duplicate ~target ~comparison =
    let open Contact in
    let%lwt () = run pool (id target |> Id.to_common) in
    find_by_contact pool target
    ||> CCList.find_opt (fun { contact; _ } ->
      Id.equal (id contact) (id comparison))
  in
  let%lwt contact_1 = create_contact ~firstname:"John" ~lastname:"Doe" in
  let%lwt contact_2 = create_contact ~firstname:"John" ~lastname:"Doe" in
  let%lwt duplicate_score =
    find_duplicate ~target:contact_1 ~comparison:contact_2
    ||> CCOption.map (fun { score; _ } -> score)
  in
  Alcotest.(
    check
      (option testable_score)
      "found duplicate with score 1"
      duplicate_score
      (Some 1.0));
  Lwt.return ()
;;
