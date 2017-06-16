%{
(* --- préambule: ici du code Caml --- *)
open Ast
open Num


let integer_suffix = 
    ["u"; "U"; "l"; "L"; "LL"; "ll"; "ul"; "uL"; "ull"; "uLL"; "Ul"; "UL"; "Ull"; "ULL"; "llu"; "llU"; "lu"; "lU"; "LLu"; "LLU"; "Lu"; "LU"]

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <string> IDENT
%token <string> OCT_CST DEC_CST HEX_CST 
%token <(Num.num * Num.num * Ast.exponent option * string)> DEC_CST_FRAC HEX_CST_FRAC
%token DOT
%start main             
                       
%type <Ast.ast list> main

%%

main:                     
    | integer_constant {print_string "ezrioh"; print_cst $1; [Const ($1)]} 
    | fractional_constant {print_cst $1; [Const ($1)]} 
    | IDENT {print_string $1; [Identifier $1]}



integer_suffix:
    | IDENT
        { if List.mem $1 integer_suffix then $1
        else failwith "an integer suffix can't be " ^ $1 }

fractional_constant:
    | DEC_CST_FRAC
        {let a, b, exp, f = $1 in
        Float(Dec, a, b, exp, f)}
    | HEX_CST_FRAC
        {let a, b, exp, f = $1 in
        Float(Hex, a, b, exp, f)}

integer_constant:
    | HEX_CST integer_suffix
        { Int(Hex, Num.num_of_string $1, $2) }
    | OCT_CST integer_suffix
        { Int(Oct, 
            (let t = String.sub $1 1 (String.length $1 - 1)
        in Num.num_of_string ("0o" ^ t)),
        $2)}
    | DEC_CST integer_suffix
        { Int(Dec, Num.num_of_string $1, $2) }
    | DEC_CST 
        { Int(Dec, Num.num_of_string $1, "") }


