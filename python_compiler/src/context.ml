open Core
open Ast

(* LLVM Initialization *)
let context = Llvm.global_context ()
let the_module = Llvm.create_module context "the_compiler"
let builder = Llvm.builder context
let named_values: (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create (module String)

(*types*)
let double_type = Llvm.double_type context
let i32_type = Llvm.i32_type context
let ch_type = Llvm.i8_type context

(*ptr types*)
let ch_ptr = Llvm.pointer_type ch_type

let debug_val = Llvm.const_int i32_type 999
let llvm_zero = Llvm.const_int i32_type 0
let llvm_i32 = Llvm.const_int i32_type
let llvm_float = Llvm.const_float double_type

(* Main Function Initialization *)
let main_fntype = Llvm.function_type i32_type [||]
let main_fn = Llvm.define_function "main" main_fntype the_module
let main_block = Llvm.entry_block main_fn 
let main_builder = Llvm.builder_at_end context main_block


(* Variables *)

let load_ptr ?(is_main=false) ptr =
  match is_main with
  | true -> Llvm.build_load ptr "load" main_builder
  | false -> Llvm.build_load ptr "load" builder

let add_var name init_val =
  Llvm.define_global name init_val the_module

let find_global_var ?(is_main=false) name =
  match Llvm.lookup_global name the_module with
  | None -> raise_s [%message "Variable does not exist"]
  | Some v_ptr -> load_ptr v_ptr ~is_main 

let find_var ?(is_main=false) name = 
  match Hashtbl.find named_values name with
  | None -> find_global_var name ~is_main 
  | Some v -> v

(* Functions *)
let arg_types args = 
  Array.create ~len:(List.length args) i32_type

let find_fn name =
  match Llvm.lookup_function name the_module with
  | None -> None
  | fn -> fn (*We don't want to redefine an existing function*)

let declare_fn fn_name fn_type =
  Llvm.declare_function fn_name fn_type the_module 

let gen_args args ~f:codegen =
  Array.map (Array.of_list args) ~f:codegen

let call ?(is_main=false) fn args = 
  match is_main with
  | true -> Llvm.build_call fn args "calltmp" main_builder
  | false -> Llvm.build_call fn args "calltmp" builder

let set_fn_args fn args = 
  Array.iteri ~f:(fun i arg ->
    let name = List.nth_exn args i in
    Llvm.set_value_name name arg;
    Hashtbl.add_exn named_values ~key:name ~data:arg;
  ) (Llvm.params fn);
    fn 

let finish_fn ret_val =
  ignore(Llvm.build_ret ret_val builder : Llvm.llvalue) 

let finish_main () =
  ignore(Llvm.build_ret llvm_zero main_builder : Llvm.llvalue) 

(* Builtin Functions *)
let is_builtin f_name =
  match f_name with
  | "print" -> true
  | _ -> false


let built_in_call ?(is_main=false) f_name args =
  match (f_name) with
  | "print" -> 
    let print_int = 
      match find_fn "print_int" with
      | None -> raise_s [%message "print_int not found"]
      | Some fn -> fn
    in
    call ~is_main print_int args
  | _ -> debug_val

(* Building Blocks *)
let make_bb the_fn =
  let bb = Llvm.append_block context "entry" the_fn in
  Llvm.position_at_end bb builder;
  ()

let gen_binop op lhs rhs =
  match op with
  | Mul -> Llvm.build_mul lhs rhs "multmp" builder
  | Div -> Llvm.build_sdiv lhs rhs "divtmp" builder
  | Mod -> Llvm.build_mul lhs rhs "multmp" builder
  | Add -> Llvm.build_add lhs rhs "addtmp" builder
  | Sub -> Llvm.build_sub lhs rhs "subtmp" builder
  | LShift -> Llvm.build_shl lhs rhs "shltmp" builder
  | RShift -> Llvm.build_ashr lhs rhs "ashrtmp" builder
  | Lt -> Llvm.build_icmp Llvm.Icmp.Slt lhs rhs "cmptmp" builder
  | Leq -> Llvm.build_icmp Llvm.Icmp.Sle lhs rhs "cmptmp" builder
  | Gt -> Llvm.build_icmp Llvm.Icmp.Sgt lhs rhs "cmptmp" builder
  | Geq -> Llvm.build_icmp Llvm.Icmp.Sge lhs rhs "cmptmp" builder
  | CmpEq -> Llvm.build_icmp Llvm.Icmp.Eq lhs rhs "cmptmp" builder
  | CmpNeq -> Llvm.build_icmp Llvm.Icmp.Ne lhs rhs "cmptmp" builder

