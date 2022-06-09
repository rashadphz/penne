open Core
open Ast

(*Optimizations*)
module PassManager = Llvm.PassManager
open Llvm_scalar_opts

(*Range from i to j*)
let (--) i j = 
  let rec aux n acc =
    if n <= i then acc else aux (n-1) (n :: acc)
  in aux j [] ;;

(* LLVM Initialization *)
let context = Llvm.global_context ()
let the_module = Llvm.create_module context "the_compiler"
let builder = Llvm.builder context

(* Optimizations *)
(* Set up the optimizer pipeline.  Start with registering info about how the
 * target lays out data structures. *)
let pass_manager = PassManager.create () 

(* Do simple "peephole" optimizations and bit-twiddling optzn. *)
let () = add_instruction_combination pass_manager
(* reassociate expressions. *)
let () = add_reassociation pass_manager
(* Eliminate Common SubExpressions. *)
let () = add_gvn pass_manager
(* Simplify the control flow graph (deleting unreachable blocks, etc). *)
let () =  add_cfg_simplification pass_manager

let () =  add_memory_to_register_promotion pass_manager

let optimize_module () = PassManager.run_module the_module pass_manager


let named_values: (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create (module String)
let named_values_ptrs: (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create (module String)

(*types*)
let double_type = Llvm.double_type context
let i32_type = Llvm.i32_type context
let ch_type = Llvm.i8_type context
let i32vector_type = Llvm.vector_type i32_type 

let list_head_idx = 0
let list_len_idx = 1
let list_cap_idx = 2

(*ptr types*)
let i32_ptr = Llvm.pointer_type i32_type
let ch_ptr = Llvm.pointer_type ch_type

(* array types *)
let i32_array = Llvm.array_type i32_type

let debug_val = Llvm.const_int i32_type 999
let llvm_zero = Llvm.const_int i32_type 0
let llvm_i32 = Llvm.const_int i32_type
let llvm_float = Llvm.const_float double_type

(* Main Function Initialization *)
let main_fntype = Llvm.function_type i32_type [||]
let main_fn = Llvm.define_function "main" main_fntype the_module
let main_block = Llvm.entry_block main_fn 
let main_builder = Llvm.builder_at_end context main_block

(* Determines if we use main_builder or builder *)
let is_main = ref false

let get_builder () = 
  match !is_main with
  | true -> main_builder
  | false -> builder

(* Variables *)
let global_exists name =
  match Llvm.lookup_global name the_module with
  | None -> false
  | Some x -> true

let load_ptr ?(name="load") ptr =
  Llvm.build_load ptr name (get_builder ())

let store_ptr ptr str_val =
  Llvm.build_store str_val ptr (get_builder ())

(* Allocate space in the stack, store the var in named_values *)
(*If var already exists just modify*)
let make_local_var name init_val =
  match Hashtbl.find named_values_ptrs name with
  | Some var_ptr -> store_ptr var_ptr init_val
  | None -> 
    let alloc_name = (name ^ "_alloc") in
    let var_ptr = Llvm.build_alloca i32_type alloc_name (get_builder ()) in
    let store = store_ptr var_ptr init_val in
    let var_value = load_ptr var_ptr ~name in
    Hashtbl.set named_values ~key:name ~data:var_value;
    Hashtbl.set named_values_ptrs ~key:name ~data:var_ptr;
    var_value

let rec alloc_fn_args args =
  match args with
  | [] -> ()
  | hd :: tl -> 
    let arg_val = Hashtbl.find_exn named_values hd in
    (make_local_var hd arg_val : Llvm.llvalue) |> ignore;
    alloc_fn_args tl;
    ()


(* If the value of a global cannot be determined at compile time *)
(* Need to determine the value then store it in memory later *)
let make_runtime_global name init_val =
  (* init_val IR has already been printed, just need to store it *)
  let global_var = Llvm.define_global name llvm_zero the_module in
  store_ptr global_var init_val


(* Non-existent -> make global, Exists -> store new value *)
let make_global_var name init_val =
  match Llvm.lookup_global name the_module with
  | None -> 
    (* Assigning to constant is easy, otherwise extra steps *)
    (match Llvm.is_constant init_val with
    | true -> Llvm.define_global name init_val the_module
    | false -> make_runtime_global name init_val)

  | Some v_ptr -> store_ptr  v_ptr init_val

let add_var name init_val =
  match !is_main with
  | true -> make_global_var name init_val
  | false -> make_local_var name init_val

let find_global_var name =
  match Llvm.lookup_global name the_module with
  | None -> raise_s [%message "Variable does not exist"]
  | Some v_ptr -> load_ptr v_ptr ~name  

(* Search the local scope first then global *)
let find_var name = 
  match Hashtbl.find named_values_ptrs name with
  | None -> find_global_var name 
  | Some v_ptr -> load_ptr v_ptr ~name:name

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

let gen_elems elems ~f:codegen =
  Array.map (Array.of_list elems) ~f:codegen

let call fn args = 
  Llvm.build_call fn args "calltmp" (get_builder ())

let set_fn_args fn args = 
  Array.iteri ~f:(fun i arg ->
    let name = List.nth_exn args i in
    Llvm.set_value_name (name ^ "_arg") arg;
    Hashtbl.add_exn named_values ~key:name ~data:arg;
  ) (Llvm.params fn);
    fn 

let finish_fn ret_val =
  Llvm.build_ret ret_val builder 

let finish_main () =
  ignore(Llvm.build_ret llvm_zero main_builder : Llvm.llvalue) 

(* Misc Helpers *)

(* Converts any llvm value to boolean *)
let to_llbool cond =
  Llvm.build_icmp Llvm.Icmp.Ne cond llvm_zero "asbool" (get_builder ())

(* Converts any llvm bool to int *)
let llint_of_bool llbool =
  Llvm.build_zext_or_bitcast llbool i32_type "bool_to_int" (get_builder ())

(* Builtin Functions *)
let is_builtin f_name =
  match f_name with
  | "print" -> true
  | _ -> false


let built_in_call f_name args =
  match (f_name) with
  | "print" -> 
    let print_int = 
      match find_fn "print_int" with
      | None -> raise_s [%message "print_int not found"]
      | Some fn -> fn
    in
    call print_int args
  | _ -> debug_val

(* Building Blocks *)
let make_bb ?(name="entry") the_fn =
  let bb = Llvm.append_block context name the_fn in
  Llvm.position_at_end bb (get_builder ()); bb

(* Lists *)
let gen_list elems =
  (* length *)
  let length = List.length elems in
  let len = Llvm.const_int i32_type length in 

  (* capacity *)
  let capacity = length in
  let cap = Llvm.const_int i32_type length in 

  let vector_type = i32vector_type capacity in
  let empty_vector = Llvm.undef vector_type in

  let rec build_vector elems idx vector =
    let ll_index = llvm_i32 idx in
    let elem = List.nth_exn elems idx in
    let updated_vector = Llvm.build_insertelement vector elem ll_index "vector" (get_builder ()) in

    if idx = length - 1 then
      updated_vector
    else
      build_vector elems (idx + 1) updated_vector
  in

  build_vector elems 0 empty_vector

let gen_access list idx =
  Llvm.build_extractelement list idx "extract" (get_builder ())


let gen_binop op lhs rhs =
  match op with
  | Mul -> Llvm.build_mul lhs rhs "multmp" (get_builder ())
  | Div -> Llvm.build_sdiv lhs rhs "divtmp" (get_builder ())
  | Mod -> Llvm.build_urem lhs rhs "modtmp" (get_builder ())
  | Add -> Llvm.build_add lhs rhs "addtmp" (get_builder ())
  | Sub -> Llvm.build_sub lhs rhs "subtmp" (get_builder ())
  | LShift -> Llvm.build_shl lhs rhs "shltmp" (get_builder ())
  | RShift -> Llvm.build_ashr lhs rhs "ashrtmp" (get_builder ())

  | Lt -> Llvm.build_icmp Llvm.Icmp.Ult lhs rhs "cmptmp" (get_builder ()) |> llint_of_bool
  | Leq -> Llvm.build_icmp Llvm.Icmp.Ule lhs rhs "cmptmp" (get_builder ()) |> llint_of_bool
  | Gt -> Llvm.build_icmp Llvm.Icmp.Ugt lhs rhs "cmptmp" (get_builder ()) |> llint_of_bool
  | Geq -> Llvm.build_icmp Llvm.Icmp.Uge lhs rhs "cmptmp" (get_builder ()) |> llint_of_bool
  | CmpEq -> Llvm.build_icmp Llvm.Icmp.Eq lhs rhs "cmptmp" (get_builder ()) |> llint_of_bool
  | CmpNeq -> Llvm.build_icmp Llvm.Icmp.Ne lhs rhs "cmptmp" (get_builder ()) |> llint_of_bool

