include One_of_n
module Countries = Countries
module Stringify = Stringify
module Database = Database

let todo _ = failwith "todo"

module Lwt_result = struct
  include Lwt_result

  module Infix = struct
    include Infix

    let ( >> ) m k = m >>= fun _ -> k
    let ( |>> ) = Lwt_result.bind_lwt
    let ( >|> ) = Lwt.bind
  end
end
