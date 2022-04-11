type t = Sihl_user.t

let equal one two = CCString.equal one.Sihl_user.id two.Sihl_user.id
let show = Sihl_user.show
let pp = Sihl_user.pp
let user m = m
