let src = Logs.Src.create "job.contact_job.event"

type event = NotifiedAbountInactivity of Contact.t [@@deriving eq, show]

let handle_event pool : event -> unit Lwt.t = function
  | NotifiedAbountInactivity contact -> Contact_job_repo.insert_notification pool contact
;;
