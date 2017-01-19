include Ppx_core
include Stdio

let with_output fn ~binary ~f =
  match fn with
  | None | Some "-" -> f stdout
  | Some fn -> Out_channel.with_file fn ~binary ~f
;;
