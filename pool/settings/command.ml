type change_settings_command = { new_settings : (string * string) list }

type handle_change_settings =
  change_settings_command -> (Event.t list, string) Result.t
