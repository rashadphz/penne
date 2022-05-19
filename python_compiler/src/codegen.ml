open Core
open Ast
open Context
open Stdlib

(* Begin Codegen *)
let rec gen_expr = function
  | Var (name) -> find_var name
  | Int (v) ->  llvm_i32 v
  | Float (v) -> llvm_float v
  | String (s) -> Llvm.const_int i32_type 999
  | BinOp (op, lhs, rhs) -> 
    let lhs_val = gen_expr lhs in
    let rhs_val = gen_expr rhs in
    gen_binop op lhs_val rhs_val
  | FunCall (f_name, f_args) -> 
    let args = gen_args f_args ~f:(gen_expr) in
    if is_builtin f_name then
      built_in_call f_name args
    else
      let callee = 
        match find_fn f_name with
        | None -> raise_s [%message "Function not found: " f_name]
        | Some callee -> callee
      in
      call callee args
  

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
  let the_fn = gen_proto proto in
    (make_bb the_fn : _) |> ignore;
  alloc_fn_args proto.args;
  let ret_val = gen_block body in (*If return value, return it else 0*)
  (finish_fn ret_val : Llvm.llvalue) |> ignore;
  (* Clear any function args/locals *)
  Hashtbl.clear named_values; 
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
  | RetVal (v) -> gen_expr v 
  | VarDecl ({name; init_val}) -> 
    let llvm_val = gen_expr init_val in
    add_var name llvm_val
  | FunDecl ({proto; body}) ->
    gen_fn proto body
  | If ({cond; true_blk; else_blk}) ->
    gen_if cond true_blk else_blk
  | While ({cond; blk}) ->
    gen_while cond blk

and gen_top_level_exp stat =
  let res = match stat with
  (* Toplevel Exp *)
  | Exp _ | If _ | While _ | VarDecl _ -> 
    is_main := true; 
    gen_statement stat 
  (* Functionlevel Exp *)

  | Block _ | RetVal _ | FunDecl _ ->
    is_main := false;
    gen_statement stat 
  in
  res

and gen_if condition true_blk else_blk = 
  let cond = gen_expr condition in
  let cond_val = to_llbool cond in 

  let start_bb = Llvm.insertion_block (get_builder ()) in
  let parent_fn = Llvm.block_parent start_bb in

  (* If *)
  let then_bb = make_bb parent_fn ~name:"then" in
  let then_val = gen_statement true_blk in
  let new_then_bb = Llvm.insertion_block (get_builder ()) in

  (* Else *)
  let else_bb = make_bb parent_fn ~name:"else" in
  let else_val = 
    match else_blk with
    | None -> llvm_zero
    | Some v -> gen_statement v in
  let new_else_bb = Llvm.insertion_block (get_builder ()) in

  (* Merge *)
  let merge_bb = make_bb parent_fn ~name:"ifcont" in
  
  (* Return to the start block to add the conditional branch. *)
  Llvm.position_at_end start_bb (get_builder ());
  (Llvm.build_cond_br cond_val then_bb else_bb (get_builder ()) : _) |> ignore;

  Llvm.position_at_end new_then_bb builder ;
  (Llvm.build_br merge_bb (get_builder ()) : _ ) |> ignore ;

  Llvm.position_at_end new_else_bb builder ;
  (Llvm.build_br merge_bb (get_builder ()) : _ ) |> ignore ;

  (* Finally, set the builder to the end of the merge block. *)
  Llvm.position_at_end merge_bb (get_builder ()) ;
  llvm_zero

and gen_while condition blk =
  let start_bb = Llvm.insertion_block (get_builder ()) in
  let parent_fn = Llvm.block_parent start_bb in

  (* Condition Block *)
  let cond_bb = make_bb parent_fn ~name:"cond" in
  let new_cond_bb = Llvm.insertion_block (get_builder ()) in
  let cond = gen_expr condition in
  let cond_val = to_llbool cond in 

  (* Body of the Loop *)
  let loop_bb = make_bb parent_fn ~name:"loop" in
  let loop_blk = gen_statement blk in
  let new_loop_bb = Llvm.insertion_block (get_builder ()) in
  (* Branch back to conditional *)
  let cond_branch = Llvm.build_br new_cond_bb (get_builder ()) in

  (* Exit Loop *)
  let exit_bb = make_bb parent_fn ~name:"cont" in

  (* Return to the start block to add the conditional branch. *)
  Llvm.position_at_end new_cond_bb (get_builder ());
  (Llvm.build_cond_br cond_val loop_bb exit_bb (get_builder ()) : _) |> ignore;

  (* Make Entry Block Branch to Condition *)
  Llvm.position_at_end start_bb (get_builder ());
  (Llvm.build_br cond_bb (get_builder ()) : _) |> ignore;

  (* Finally, set the builder to the end of the continue block. *)
  Llvm.position_at_end exit_bb (get_builder ()) ;

  llvm_zero


let rec gen_prog prog =
  match prog with 
  | Prog [] -> ignore(finish_main () : _)
  | Prog (hd :: tl) -> 
    let (_ : Llvm.llvalue) = gen_top_level_exp hd in
    gen_prog (Prog tl)


let gen_std_lib () =
  ignore (printf_fn);
  ignore (gen_print_int (): _)