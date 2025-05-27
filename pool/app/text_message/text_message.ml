include Entity
include Event
module Service = Text_message_service

let find_report_by_queue_id = Repo.find_report_by_queue_id
