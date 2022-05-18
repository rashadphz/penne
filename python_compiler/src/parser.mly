%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE

%token TIMES DIV MOD
%token PLUS MINUS

%token LSHIFT RSHIFT

%token LT LEQ GT GEQ
%token CMP_EQ CMP_NEQ

%token COLON
%token EOF
%token DEF

%token TRUE FALSE
%token WHILE FOR

%token COMMA
%token SEMICOLON
%token RETURN

%token EQ

%token IF ELSE ELIF
%token PRINT

%start <Ast.prog> prog
%%


prog:
  | s = statement; p = prog;
    { let Prog ss = p in Prog ( s :: ss ) } (* Top-lvl-exp followed by rest of program*)
  | EOF { Prog [] }


expr:
  | i = INT { Int i }
  | f = FLOAT { Float f }
  | x = ID { Var x }
  | e1 = expr; op = binop; e2 = expr { BinOp (op, e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
  | id = ID; LPAREN; args = args; RPAREN; 
    { FunCall (id, args) }
  
var_decl:
  id = ID; EQ; e = expr
    { { name = id; init_val = e } }

statement:
  | e = expr; SEMICOLON
    { Exp (e) }
  | vd = var_decl; SEMICOLON
    { VarDecl (vd) }
  | fd = fun_def;
    { FunDecl (fd) }
  | IF; cond = expr; COLON; true_blk = statement; else_blk = if_else_blk
    { If {cond; true_blk; else_blk} }
  | b = block
    { Block b }
  | RETURN; ret_val = expr; SEMICOLON;
    { RetVal ret_val }

statements:
  | { [] }
  | s = statement ss = statements
    { s :: ss }

block:
  | LBRACE; stmnts = statements; RBRACE
    { stmnts }

fun_proto:
  DEF; fun_name = ID; LPAREN; args = fn_args; RPAREN;
  { {fun_name; args} }

fun_def:
  proto = fun_proto; COLON; body = block
    { {proto; body} }

fn_args:
  | { [] }
  | id = ID
    { [id] }
  | id = ID; COMMA; ps = fn_args
    { id :: ps }

args:
  | { [] }
  | expr = expr { [expr] }
  | e = expr; COMMA; es = args
    { e :: es }

if_else_blk:
  | { None }
  // | ELIF; cond = expr; body = statement; { Some {cond; body} }
  | ELSE; COLON; body = statement; { Some body }

%inline binop:
  | TIMES { Mul }
  | DIV { Div }
  | MOD { Mod }
  | PLUS { Add }
  | MINUS { Sub }
  | LSHIFT { LShift }
  | RSHIFT { RShift }
  | LT { Lt }
  | LEQ { Leq }
  | GT { Gt }
  | GEQ { Geq }
  | CMP_EQ { CmpEq }
  | CMP_NEQ { CmpNeq }

%%