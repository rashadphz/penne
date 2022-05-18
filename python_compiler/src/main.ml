open Core

let read_file fname =
  In_channel.read_all fname

let input_fname = (Sys.get_argv ()).(1)

(* Run Unix Command and store the output in a list *)
(* let list_of_output command = 
  let in_chan = Unix.open_process_in command in
  let all_input = ref [] in
  let rec make_input () =
    all_input := In_channel.input_line in_chan :: !all_input;
    make_input () 
  in
  try make_input ()
  with End_of_file ->
    let stat = Unix.close_process_in in_chan in 
    (List.rev !all_input, stat)

let list_of_cmd command =
  let (lst, _) = list_of_output command in lst *)

(* let print_endline_opt str =
  let str = match str with
  | None -> "Error"
  | Some str -> str 
  in print_endline str *)

(* let print_output () =
  let output = list_of_cmd "lli test.ll" in
  List.iter ~f:(print_endline_opt) output *)

(* let print_ast ast =
  ast |> string_of_prog |> print_string *)

let main () =
  (* Build Stdlib *)
  ignore(Codegen.gen_std_lib () : _);

  (* Build Ast *)
  let lexbuf = Lexing.from_string (read_file input_fname) in
  let ast = Parser.prog Lexer.read lexbuf in
  ast |> Codegen.gen_prog;

  Llvm.print_module "test.ll" Context.the_module;
  Llvm.dump_module Context.the_module


let () = main ();
