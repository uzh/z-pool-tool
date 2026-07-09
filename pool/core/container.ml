(* Extracted from the Sihl web framework (https://github.com/oxidizing/sihl), MIT license.
   Source:
   https://github.com/uzh/sihl/blob/6c3c4040413294155cda3a363edf0ff3c7e638b8/sihl/src/core_container.ml *)

include Lifecycle
module Service = Service

let start_services = Service.start_services
let stop_services = Service.stop_services
