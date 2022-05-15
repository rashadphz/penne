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

type assignop =
  | AssignEq

type expr = 
  | Var of string
  | Int of int
  | Float of float
  | String of string
  | BinOp of bop * expr * expr
  | FunCall of string * expr list (*fun_name by fun_args*)

type var_decl = {
  var_name: string;
  init_val: expr;
}

type fun_proto = {
  fun_name: string;
  params: string list;
}

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
  | VarDecl of var_decl
  | FunDecl of fun_def
  and fun_def = {
    proto: fun_proto;
    body: block;
  }

type prog = Prog of statement list

