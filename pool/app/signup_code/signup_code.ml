include Entity
include Entity_guard
include Event
include Repo

module Webhook = struct
  include Entity.Webhook
  module Job = Webhook_job
end
