open Libojs.Driver

let fname = ref None

let () = Arg.parse [] (fun f -> fname := Some f) "usage: ojs fname"

let _ = main !fname |> exit
