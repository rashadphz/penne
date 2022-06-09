{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

  let keyword_table = Hashtbl.create 50
  let () = 
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) 
    ["def", DEF;
    "True", TRUE;
    "False", FALSE;
    "while", WHILE;
    "for", FOR;
    "return", RETURN;
    "if", IF;
    "else", ELSE;
    "elif" , ELIF;
    "range" , RANGE;
    "in", IN;
    ]

  let consume_identifier id = 
    match Hashtbl.find_opt keyword_table id with
    | Some tok -> tok
    | None -> ID id
}

let digit = ['0'-'9']
let int = ['-']* digit+
let float = ['-']* digit+ '.' digit+

let id = ['a'-'z' 'A'-'Z' '_']+ ['a'-'z' 'A'-'Z' '_' '0' -'9']*

let white_space = [' ']+
let new_line = ['\n' '\r']+

rule read = parse 
  (* | white_space {read lexbuf} *)
  (* | new_line {next_line lexbuf; read lexbuf} *)
  | '\r' { read lexbuf }
  | '\n'+ { NEWLINE }
  | '\t' { TAB }
  | '#' { comment lexbuf }
  | ' ' { SPACE }
  | "*" { TIMES }
  | "/" { DIV }
  | "%" { MOD }
  | "+" { PLUS }
  | "-" { MINUS }
  | "<<" { LSHIFT }
  | ">>" { RSHIFT }
  | "," { COMMA }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "<" { LT }
  | "<=" { LEQ }
  | ">" { GT }
  | ">=" { GEQ }
  | "==" { CMP_EQ }
  | "!=" { CMP_NEQ }
  | "=" { EQ }
  | ";" { SEMICOLON }
  | ":" { COLON }
  | id as id { consume_identifier id }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  | eof {EOF}
  | _ as char { Core.raise_s[%message "Invalid Character: " (Char.escaped char) ]}

and comment = parse (* continue until newline *)
  | '\n' { NEWLINE }
  | _ { comment lexbuf }