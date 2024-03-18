module Admin = struct
  let session_path ?suffix experiment_id session_id =
    let default =
      Format.asprintf
        "/admin/experiments/%s/sessions/%s"
        Experiment.(Id.value experiment_id)
        Session.(session_id |> Id.value)
    in
    suffix |> CCOption.map_or ~default (Format.asprintf "%s/%s" default)
  ;;
end
