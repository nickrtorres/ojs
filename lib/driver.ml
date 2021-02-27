open Lexer
open Interpreter
open Types

let main fname =
  try
    let ic = match fname with Some f -> open_in f | None -> stdin in
    let lexbuf = Lexing.from_channel ic in
    let program = Parser.program Lexer.token lexbuf in
    let completion = run program in
    let () = Printf.printf "%s\n" (string_of_completion completion) in
    let () = close_in ic in
    0
  with Lex_error (lnum, token) ->
    let () = Printf.printf "error: line %d: invalid token: %s\n" lnum token in
    1
