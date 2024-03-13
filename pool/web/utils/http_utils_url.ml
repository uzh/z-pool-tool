module Admin = struct
  let session_path ?suffix experiment_id session_id =
    let path =
      Format.asprintf
        "/admin/experiments/%s/sessions/%s"
        Experiment.(Id.value experiment_id)
        Session.(session_id |> Id.value)
    in
    suffix |> CCOption.map_or ~default:path (Format.asprintf "%s/%s" path)
  ;;
end
