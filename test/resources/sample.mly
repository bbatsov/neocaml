(* A sample Menhir parser *)

%{
  open Ast

  let make_binop op e1 e2 =
    Binop (op, e1, e2)
%}

%token<int> INT
%token<string> STRING
%token EOF PLUS MINUS STAR SLASH
%token LPAREN RPAREN
%token LET EQUAL IN

%left PLUS MINUS
%left STAR SLASH

%start<Ast.expr> main
%type<Ast.expr> expr

%%

main:
  | e = expr EOF { e }

expr:
  | i = INT { Int i }
  | s = STRING { String s }
  | LPAREN e = expr RPAREN { e }
  | e1 = expr PLUS e2 = expr { make_binop Add e1 e2 }
  | e1 = expr MINUS e2 = expr { make_binop Sub e1 e2 }
  | e1 = expr STAR e2 = expr { make_binop Mul e1 e2 }
  | e1 = expr SLASH e2 = expr { make_binop Div e1 e2 }
  | LET x = STRING EQUAL e1 = expr IN e2 = expr { Let (x, e1, e2) }
