let get_random list =
  let n = Random.int (CCList.length list) in
  CCList.nth list n
;;
