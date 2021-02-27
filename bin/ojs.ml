open Libojs.Driver

let fname = ref None

let () = Arg.parse [] (fun f -> fname := Some f) "usage: ojs fname"

let _ = match !fname with Some f -> main f |> exit | None -> repl () |> exit
