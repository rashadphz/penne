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
%token LBRACKET
%token RBRACKET

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
%token IN RANGE

%token COMMA
%token SEMICOLON
%token RETURN

%token EQ

%token IF ELSE ELIF
%token PRINT

%token TAB SPACE NEWLINE
%token INDENT DEDENT

(*Operator Precedence*)
%left PLUS MINUS
%left TIMES DIV MOD

(* Get the tokens and reformat with proper indentation *)
%start <token list> tokenize

(* Use the formatted tokens to build AST *)
%start <Ast.prog> prog
%%


token:
  | INT { INT ($1) }
  | FLOAT { FLOAT ($1) }
  | ID { ID ($1) }
  | TIMES { TIMES }
  | DIV { DIV }
  | MOD { MOD }
  | PLUS { PLUS }
  | MINUS { MINUS }
  | LSHIFT { LSHIFT }
  | RSHIFT { RSHIFT }
  | COMMA { COMMA }
  | LPAREN { LPAREN }
  | RPAREN { RPAREN }
  | LBRACE { LBRACE }
  | RBRACE { RBRACE }
  | LBRACKET { LBRACKET}
  | RBRACKET { RBRACKET }
  | LT { LT }
  | LEQ { LEQ }
  | GT { GT }
  | GEQ { GEQ }
  | CMP_EQ { CMP_EQ }
  | CMP_NEQ { CMP_NEQ }
  | EQ { EQ }
  | SEMICOLON { SEMICOLON }
  | COLON { COLON }
  | NEWLINE { NEWLINE }
  | SPACE { SPACE }
  | TAB { TAB }
  | IF { IF }
  | ELSE { ELSE }
  | WHILE { WHILE }
  | DEF { DEF }
  | RETURN { RETURN }
  | COMMA { COMMA }
  | FOR { FOR }
  | IN { IN }

tokenize:
  | tl = toklist
    { tl }

toklist:
  | t = token; ts = toklist
    { t :: ts }
  | EOF { [] }

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
  | LBRACKET; elems = args; RBRACKET;
    { List (elems) }
  | list = ID; LBRACKET; idx = expr; RBRACKET;
    { ListAccess (list, idx)}
  
var_decl:
  id = ID; EQ; e = expr
    { { name = id; init_val = e } }

statement:
  | e = expr; NEWLINE;
    { Exp (e) }
  | vd = var_decl; NEWLINE;
    { VarDecl (vd) }
  | fd = fun_def;
    { FunDecl (fd) }
  | IF; cond = expr; COLON; true_blk = statement; else_blk = if_else_blk
    { If {cond; true_blk; else_blk} }
  | b = block
    { Block b }
  | RETURN; ret_val = expr; NEWLINE;
    { RetVal ret_val }
  | WHILE; cond = expr; COLON; blk = statement;
    {While {cond; blk} }
  | FOR; var_name = ID; IN; sequence = expr; COLON; blk = statement;;
    { For {var_name; sequence; blk } } 

statements:
  | { [] }
  | s = statement ss = statements
    { s :: ss }

block:
  | NEWLINE; INDENT ; stmnts = statements; DEDENT
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