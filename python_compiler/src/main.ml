open Core
open Tokutils
open !Cmdliner

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

let print_ast ast =
  ast |> Ast.string_of_prog |> print_string

let print_tokens tokens =
  List.iter ~f:(fun (tok) -> (string_of_tok tok) ^ ", " |> print_string) tokens 

type comparison = Lt | Eq | Gt

let comp a b =
  if a < b then Lt
  else if a > b then Gt
  else Eq

(*We need to skip space tokens that are not at the start of the line*)
let is_line_start = ref false

(* https://docs.python.org/3/reference/lexical_analysis.html#indentation *)
let sanitize tokens =
  let rec indent tokens stack indent_lvl output =
    match tokens with
    | [] -> 
      (*Place dedents until stack is empty*)
      let rec final_dedents output stack = 
        match Stack.top_exn stack = 0 with
        | true -> output
        | false -> 
          (Stack.pop_exn stack : int ) |> ignore;
          final_dedents (Parser.DEDENT :: output) stack
      in
      final_dedents output stack
    | Parser.NEWLINE :: ts ->
      is_line_start := true;
      indent ts stack 0 (Parser.NEWLINE :: output)
    | Parser.SPACE :: ts -> 
      if !is_line_start then
      indent ts stack (indent_lvl + 1) output
      else
      indent ts stack (indent_lvl) output
    | tok :: ts -> 
      is_line_start := false;
      let top = Stack.top_exn stack in
      match comp indent_lvl top with
      | Lt -> 
        let dedent_out = dedent stack indent_lvl output in
        indent ts stack indent_lvl (tok :: dedent_out)
      | Eq -> indent ts stack indent_lvl (tok :: output)  (* Do nothing *)
      | Gt -> 
        Stack.push stack indent_lvl;
        indent ts stack indent_lvl (tok :: Parser.INDENT :: output)

  and dedent stack indent_lvl output =
    let top = Stack.top_exn stack in
    match comp top indent_lvl with
    | Lt | Eq -> output
    | Gt -> 
      (Stack.pop_exn stack : int) |> ignore;
      dedent stack indent_lvl (Parser.DEDENT :: output)
  in
  let stack = Stack.create () in 
  Stack.push stack 0;
  let clean_tokens = indent tokens stack 0 []
  in List.rev (Parser.EOF :: clean_tokens)


let penne fname =
  (* Build Stdlib *)
  ignore(Codegen.gen_std_lib () : _);

  (* Build Token List*)
  let lexbuf = Lexing.from_string ((read_file fname) ^ "\n") in
  let token_list = Parser.tokenize Lexer.read lexbuf in
  let clean_list = ref (token_list |> sanitize) in 

  (*trick to pass tokenlist through parser*)
  let token lexbuf = 
    match !clean_list with 
      | [] -> Parser.EOF 
      | h :: t -> 
        clean_list := t ; h  
  in

  (* Build Ast *)
  let ast = Parser.prog token (Lexing.from_string "") in 
  (* ast |> print_ast; *)
  ast |> Codegen.gen_prog;

  (Context.optimize_module () : bool) |> ignore;
  Llvm.print_module "test.ll" Context.the_module;
  (Sys.command("lli test.ll") : int) |> ignore
  (* Llvm.dump_module Context.the_module *)

let () = Cli.penne_cli ~f:(penne) 
