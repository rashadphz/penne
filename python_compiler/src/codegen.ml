open Core
open Ast
open Context
open Stdlib

(* Begin Codegen *)
let rec gen_expr ?(is_main=false) = function
  | Var (name) -> find_var ~is_main name
  | Int (v) ->  llvm_i32 v
  | Float (v) -> llvm_float v
  | String (s) -> Llvm.const_int i32_type 999
  | BinOp (op, lhs, rhs) -> 
    let lhs_val = gen_expr lhs ~is_main in
    let rhs_val = gen_expr rhs ~is_main in
    gen_binop op lhs_val rhs_val
  | FunCall (f_name, f_args) -> 
    let args = gen_args f_args ~f:(gen_expr ~is_main) in
    if is_builtin f_name then
      built_in_call ~is_main f_name args
    else
      let callee = 
        match find_fn f_name with
        | None -> raise_s [%message "Function not found: " f_name]
        | Some callee -> callee
      in
      call ~is_main callee args

let gen_proto proto = 
    let fun_name = proto.fun_name in
    let args = proto.args in

    let a_types = arg_types args in
    let fn_type = Llvm.function_type i32_type a_types in
    let fn =  
      match find_fn fun_name with
      | None -> declare_fn fun_name fn_type
      | Some (fn) -> raise_s [%message "Function already exists" (fun_name) ]
    in set_fn_args fn args

let rec gen_fn proto body =  
  Hashtbl.clear named_values; (*Need to reset args for new function*)
  let the_fn = gen_proto proto in
    make_bb the_fn;
  let ret_val = gen_block body in
    finish_fn ret_val;
  the_fn

(*generate a statement then the rest of the block*)
and gen_block block =
   match block with
   | [] -> llvm_zero
   | [s] -> gen_statement s
   | s :: ss -> ignore(gen_statement s : Llvm.llvalue); gen_block ss

and gen_statement = function
  | Exp (e) -> gen_expr e
  | Block (ss) -> gen_block ss
  | RetVal (ret) -> ret |> gen_expr 
  | VarDecl ({name; init_val}) -> 
    let llvm_val = gen_expr init_val in
    add_var name llvm_val
  | FunDecl ({proto; body}) ->
    gen_fn proto body;
  | _ -> debug_val

and gen_top_level_exp stat =
  match stat with
  | Exp (e) -> gen_expr e ~is_main:true
  | _ -> gen_statement stat

let rec gen_prog prog =
  match prog with 
  | Prog [] -> ignore(finish_main () : _)
  | Prog (hd :: tl) -> 
    let (_ : Llvm.llvalue) = gen_top_level_exp hd in
    gen_prog (Prog tl)


let gen_std_lib () =
  ignore (printf_fn);
  ignore (gen_print_int (): _)