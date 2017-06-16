%{
(* --- préambule: ici du code Caml --- *)
open Ast
open Num


let integer_suffix = 
    ["u"; "U"; "l"; "L"; "LL"; "ll"; "ul"; "uL"; "ull"; "uLL"; "Ul"; "UL"; "Ull"; "ULL"; "llu"; "llU"; "lu"; "lU"; "LLu"; "LLU"; "Lu"; "LU"]

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <string> IDENT STRING_LIT
%token <Ast.constant> CONSTANT
%token SIZEOF
%start main             
                       
%type <Ast.ast list> main

%%

main:                     
    | CONSTANT 
        { [Constant $1] }

identifier:
    | IDENT
        { Identifier ($1) }

primary_expression:
    | identifier
        { $1 }
    | CONSTANT
        { Constant $1 }
    | STRING_LIT 
        { String $1 }
    | '(' expression ')'
        { $2 }


postfix_expression:
    | primary_expression
        { $1 }
    | postfix_expression '[' expression ']'
        { Access(Array, $1, $3)}
    | postfix_expression '(' argument_expression_list ')'
        { Call($1, List.rev $3)}
    | postfix_expression '(' ')'
        { Call($1, [])}
    | postfix_expression '.' identifier
        { Access(Member, $1, $3)}
    | postfix_expression "->" identifier
        { Access(Pointer, $1, $3)}
    | postfix_expression "++" 
        { UnaryOp(UnOp.PostIncr, $1)}
    | postfix_expression "--" 
        { UnaryOp(UnOp.PostDecr, $1)}
    
    | '(' type_name ')' '{' initializer_list '}'
        { Cast($2, InitializerList $5) }
    | '(' type_name ')' '{' initializer_list ',' '}'
        { Cast($2, InitializerList $5) }
     
argument_expression_list:
    | assignement_expression
        { [$1] }
    | argument_expression_list assignement_expression
        { $2 :: $1 }

unary_operator:
    | '*' { UnOp.DeRef }
    | '&' { UnOp.Ref }
    | '!' { UnOp.Not }
    | '~' { UnOp.Neg }
    | '-' { UnOp.Sub }
    | '+' { UnOp.Add }
unary_expression:
    | postfix_expression
        { $1 }
    | "++" unary_expression
        { UnaryOp(UnOp.PreIncr, $2) }
    | "--" unary_expression
        { UnaryOp(UnOp.PreDecr, $2) }
    | unary_operator unary_expression
        { UnaryOp($1, $2) }
    | SIZEOF '(' type_name ')'
        { UnaryOp(UnOp.SizeOf, Type $3) }
    | SIZEOF unary_expression
        { UnaryOp(UnOp.SizeOf, $2) }
 
cast_expression:
    | unary_expression
        { $1 }
    | '(' type_name ')' cast_expression
        { Cast($2, SeldomArg $4) }


multiplicative_expression:
    | cast_expression
        { $1 }
    | multiplicative_expression '*' cast_expression 
        { BinaryOp(BinOp.Mul, $1, $3) }
    | multiplicative_expression '/' cast_expression 
        { BinaryOp(BinOp.Div, $1, $3) }
    | multiplicative_expression '%' cast_expression 
        { BinaryOp(BinOp.Mod, $1, $3) }

additive_expression:
    | multiplicative_expression
        { $1 }
    | additive_expression '-' multiplicative_expression 
        { BinaryOp(BinOp.Sub, $1, $3) }
    | additive_expression '+' multiplicative_expression 
        { BinaryOp(BinOp.Add, $1, $3) }

shift_expression: 
    | additive_expression
        { $1 }
    | shift_expression "<<" additive_expression
        { BinaryOp(BinOp.LShift, $1, $3) }
    | shift_expression ">>" additive_expression
        { BinaryOp(BinOp.RShift, $1, $3) }

relational_expression: 
    | shift_expression
        { $1 }
    | relational_expression "<" shift_expression
        { BinaryOp(BinOp.Slt, $1, $3) }
    | relational_expression ">" shift_expression
        { BinaryOp(BinOp.Sgt, $1, $3) }
    | relational_expression "<=" shift_expression
        { BinaryOp(BinOp.Leq, $1, $3) }
    | relational_expression "=>" shift_expression
        { BinaryOp(BinOp.Geq, $1, $3) }

equality_expression:
    | relational_expression
        { $1 }
    | equality_expression "==" relational_expression
        { BinaryOp(BinOp.Eq, $1, $3) }
    | equality_expression "!=" relational_expression
        { BinaryOp(BinOp.Neq, $1, $3) }

and_expression:
    | equality_expression
        { $1 }
    | and_expression "&" equality_expression
        { BinaryOp(BinOp.BinAnd, $1, $3)}
xor_expression:
    | and_expression
        { $1 }
    | xor_expression "^" and_expression
        { BinaryOp(BinOp.BinXor, $1, $3)}
or_expression:
    | xor_expression
        { $1 }
    | or_expression "|" xor_expression
        { BinaryOp(BinOp.BinXor, $1, $3)}
logical_and_expression:
    | or_expression
        { $1 }
    | logical_and_expression "&&" or_expression
        { BinaryOp(BinOp.And, $1, $3)}
logical_or_expression:
    | logical_and_expression
        { $1 }
    | logical_or_expression "||" logical_and_expression
        { BinaryOp(BinOp.Or, $1, $3)}

conditional_expression:
    | logical_or_expression
        { $1 }
    | logical_or_expression '?' expression ':' conditional_expression
        { IfThenElse(Ternary, $1, $3, $5) }

assignement_operator:
    | "=" { BinOp.Empty }
    | "*=" { BinOp.Mul }
    | "/=" { BinOp.Div }
    | "%=" { BinOp.Mod }
    | "+=" { BinOp.Add }
    | "-=" { BinOp.Sub }
    | "<<=" { BinOp.LShift }
    | ">>=" { BinOp.RShift }
    | "&=" { BinOp.BinAnd }
    | "^=" { BinOp.BinXor }
    | "|=" { BinOp.BinOr }

assignement_expression:
    | conditional_expression
        { $1 }
    | unary_expression assignement_operator assignement_expression
        { Assign($2, $1, $3) }

/* must return it */
expression:
    | assignement_expression
        { $1 }
    | expression "," assignement_expression
        { match $1 with
        | Expression l -> Expression ($3 :: l)
        | _ -> Expression($3 :: [$1])
        }

type_name:
    | "int"
        { Int }
initializer_list:
    | 'a'
        {[]}

constant_expression:
    | conditional_expression
        { $1 }
