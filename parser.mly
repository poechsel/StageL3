%{
(* --- préambule: ici du code Caml --- *)
open Ast
open Num


let integer_suffix = 
    ["u"; "U"; "l"; "L"; "LL"; "ll"; "ul"; "uL"; "ull"; "uLL"; "Ul"; "UL"; "Ull"; "ULL"; "llu"; "llU"; "lu"; "lU"; "LLu"; "LLU"; "Lu"; "LU"]

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <string> IDENT
%token <string> OCT_CST DEC_CST HEX_CST  CHAR_CST
%token <(Num.num * Num.num * Ast.exponent option * string)> DEC_CST_FRAC HEX_CST_FRAC
%token <Ast.constant> CONSTANT
%token DOT
%start main             
                       
%type <Ast.ast list> main

%%

main:                     
    | CONSTANT 
        { [Const $1] }



