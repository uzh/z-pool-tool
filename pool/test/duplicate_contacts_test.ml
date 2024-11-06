module ContactRepo = Integration_utils.ContactRepo

let pool = Test_utils.Data.database_label
let get_exn = Test_utils.get_or_failwith
let testable_score = Alcotest.(testable CCFloat.pp CCFloat.equal)

let check_similarity _ () =
  let open Utils.Lwt_result.Infix in
  let open Duplicate_contacts in
  let open Pool_user in
  let check ~result ~expected msg =
    Alcotest.check (Alcotest.option testable_score) msg result expected
  in
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
    ||> fst
    ||> CCList.find_opt (fun { contact_a; contact_b; _ } ->
      Id.equal (id contact_a) (id comparison)
      || Id.equal (id contact_b) (id comparison))
  in
  let%lwt contact_1 = create_contact ~firstname:"John" ~lastname:"Doe" in
  let%lwt contact_2 = create_contact ~firstname:"John" ~lastname:"Doe" in
  let%lwt duplicate_score =
    find_duplicate ~target:contact_1 ~comparison:contact_2
    ||> CCOption.map (fun { score; _ } -> score)
  in
  check
    ~result:duplicate_score
    ~expected:(Some 1.0)
    "found duplicate with score 1";
  let%lwt contact_3 = create_contact ~firstname:"Jane" ~lastname:"Doe" in
  let%lwt duplicate_score =
    find_duplicate ~target:contact_1 ~comparison:contact_3
    ||> CCOption.map (fun { score; _ } -> score)
  in
  check
    ~result:duplicate_score
    ~expected:None
    "do not find duplicate with different given name";
  Lwt.return ()
;;
