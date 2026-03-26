(* A sample OCamllex lexer *)

{
  open Lexing

  exception Syntax_error of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = pos.pos_cnum;
                 pos_lnum = pos.pos_lnum + 1 }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | digit | '_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | whitespace { token lexbuf }
  | newline { next_line lexbuf; token lexbuf }
  | digit+ as n { INT (int_of_string n) }
  | digit+ '.' digit* as f { FLOAT (float_of_string f) }
  | ident as id { IDENT id }
  | '"' { read_string (Buffer.create 17) lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '=' { EQUAL }
  | eof { EOF }
  | _ as c { raise (Syntax_error (Printf.sprintf "Unexpected char: %c" c)) }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | _ as c { Buffer.add_char buf c; read_string buf lexbuf }
  | eof { raise (Syntax_error "String not terminated") }

{
  let () =
    let lexbuf = Lexing.from_channel stdin in
    try
      while true do
        let _ = token lexbuf in ()
      done
    with End_of_file -> ()
}
