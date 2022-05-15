open Ast
open Core

let read_file fname =
  In_channel.read_all fname

let input_fname = (Sys.get_argv ()).(1)

let () =
  let lexbuf = Lexing.from_string (read_file input_fname) in
  let ast = Parser.prog Lexer.read lexbuf in
  ast |> string_of_prog |> print_string

