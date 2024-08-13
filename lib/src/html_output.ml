open! Core
open! Import
module Unix = Core_unix

include Patdiff_kernel.Html_output.Private.Make (struct
    let mtime file =
      let%map.Or_error stats =
        Or_error.try_with (fun () -> Unix.stat (File_name.real_name_exn file))
      in
      stats.st_mtime |> Time_float.Span.of_sec |> Time_float.of_span_since_epoch
    ;;
  end)
