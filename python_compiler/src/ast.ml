open Core

type bop = 
  | Mul
  | Div
  | Mod
  | Add
  | Sub
  | LShift
  | RShift
  | Lt
  | Leq
  | Gt
  | Geq
  | CmpEq
  | CmpNeq
[@@deriving sexp]

type assignop =
  | AssignEq
[@@deriving sexp]

type expr = 
  | Var of string
  | Int of int
  | Float of float
  | String of string
  | BinOp of bop * expr * expr
  | FunCall of string * expr list (*fun_name by fun_args*)
[@@deriving sexp]

type var_decl = {
  name: string;
  init_val: expr;
}
[@@deriving sexp]

type fun_proto = {
  fun_name: string;
  args: string list;
}
[@@deriving sexp]

type block = statement list
and statement =
  | Exp of expr
  | Block of block
  | RetVal of expr
  | If of {
      cond: expr;
      true_blk: statement;
      else_blk: statement option;
    }
  | While of {
    cond: expr;
    blk: statement;
  }
  | VarDecl of var_decl
  | FunDecl of fun_def
  and fun_def = {
    proto: fun_proto;
    body: block;
  }
[@@deriving sexp]

type prog = Prog of statement list
[@@deriving sexp]

let string_of_prog prog = prog |> sexp_of_prog |> Sexp.to_string_hum 

