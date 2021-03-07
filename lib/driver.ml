open Lexer
open Interpreter
open Types

let main fname =
  try
    let ic = open_in fname in
    let lexbuf = Lexing.from_channel ic in
    let program = Parser.program Lexer.token lexbuf in
    let ctx = Ctx.new_ctx () in
    let _ = run program ctx in
    let () = close_in ic in
    0
  with Lex_error (lnum, token) ->
    let () = Printf.printf "error: line %d: invalid token: %s\n" lnum token in
    1

let repl () =
  try
    let ctx = Ctx.new_ctx () in
    let rec repl' () =
      let () = print_string "# " in
      let lexbuf = Lexing.from_string (read_line ()) in
      let program = Parser.program Lexer.token lexbuf in
      let completion = run program ctx in
      let () = Printf.printf "%s\n" (string_of_completion completion) in
      repl' ()
    in

    repl' ()
  with End_of_file -> 0
