%{
(* --- préambule: ici du code Caml --- *)
open Ast
open Num


let integer_suffix = 
    ["u"; "U"; "l"; "L"; "LL"; "ll"; "ul"; "uL"; "ull"; "uLL"; "Ul"; "UL"; "Ull"; "ULL"; "llu"; "llU"; "lu"; "lU"; "LLu"; "LLU"; "Lu"; "LU"]

let extract_bloc b = match b with
    | Bloc b -> b
    | b -> [b]

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <string> IDENT STRING_LIT
%token <Ast.constant> CONSTANT
%token SIZEOF ENDLINE TYPEDEF EXTERN STATIC AUTO REGISTER VOID CHAR INT SHORT LONG FLOAT DOUBLE SIGNED UNSIGNED BOOL COMPLEX STRUCT UNION ENUM CONST VOLATILE RESTRICT INLINE
%token CASE SWITCH GOTO FOR WHILE IF ELSE BREAK CONTINUE RETURN DEFAULT DO
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
    | assignment_expression
        { [$1] }
    | argument_expression_list assignment_expression
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
        { IfThenElse(Ternary, $1, [$3], [$5]) }

assignment_operator:
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

assignment_expression:
    | conditional_expression
        { $1 }
    | unary_expression assignment_operator assignment_expression
        { Assign($2, $1, $3) }

/* must return it */
expression:
    | assignment_expression
        { $1 }
    | expression "," assignment_expression
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









/*
declaration:
    | declaration_specifiers ENDLINE
        { Declaration($1, []) }
    | declaration_specifiers init_declarator_list ENDLINE
        { Declaration($1, List.rev $2) }

declaration_specifiers:
    | storage_class_specifiers declaration_specifiers
        { $1 :: $2 }
    | storage_class_specifiers 
        { $1 :: [] }
    | type_qualifier declaration_specifiers
        { $1 :: $2 }
    | type_qualifier 
        { $1 :: [] }
    | type_specifier declaration_specifiers
        { $1 :: $2 }
    | type_specifier 
        { $1 :: [] }
    | function_specifier declaration_specifiers
        { $1 :: $2 }
    | function_specifier
        { $1 :: [] }
        
init_declarator_list:
    | init_declarator
        { [$1] }
    | init_declarator_list "," init_declarator
        { $2::$1 }

init_declarator:
    | declarator
        { DDeclaration ($1, None) }
    | declarator "=" initializer
        { DDeclaration( $1, Some $3)}

storage_class_specifier:
    | TYPEDEF   { Typedef  }
    | EXTERN    { Extern   }
    | STATIC    { Static   }
    | AUTO      { Auto     }
    | REGISTER  { Register }

type_specifier:
    | VOID      { Void     }
    | CHAR      { Char     }
    | SHORT     { Short    }
    | INT       { Int      }
    | LONG      { Long     }
    | FLOAT     { Float    }
    | DOUBLE    { Double   }
    | SIGNED    { Signed   }
    | UNSIGNED  { Unsigned }
    | BOOL      { Bool     }
    | COMPLEX   { Complex  }
    | struct_or_union_specifier 
        { $1 }
    | enum_specifier
        { $1 }
    | typedef_name 
        { $1 }

struct_or_union_specifier:
    | STRUCT IDENT "{" struct_declaration_list "}"
        { Struct($2, List.rev $4) }
    | STRUCT "{" struct_declaration_list "}"
        { Struct("", List.rev $3) }
    | STRUCT IDENT 
        { Struct($2, []) }

    | UNION IDENT "{" struct_declaration_list "}"
        { Union($2, List.rev $4) }
    | UNION "{" struct_declaration_list "}"
        { Union("", List.rev $3) }
    | UNION IDENT 
        { Union($2, []) }

struct_declaration_list:
    | struct_declaration
        { [$1] }
    | struct_declaration_list struct_declaration
        { $2::$1 }

struct_declaration:
    | specifier_qualifier_list struct_declarator_list ENDLINE
        { ($1, List.rev $2) }

specifier_qualifier_list:
    | type_specifier specifier_qualifier_list
        { $1::$2 }
    | type_specifier 
        { [$1] }
    | type_qualifier specifier_qualifier_list
        { $1::$2 }
    | type_qualifier 
        { [$1] }

struct_declarator_list:
    | struct_declarator
        { $1 }
    | struct_declarator_list struct_declarator
        { $2 :: $1 }

struct_declarator:
    | declarator
    | ":" constant_expression
    | declarator ":" constant_expression


enum_specifier:
    | ENUM IDENT "{" enumerator_list "}"
        { Enum($2, List.rev $4)}
    | ENUM "{" enumerator_list "}"
        { Enum("", List.rev $3)}
    | ENUM IDENT "{" enumerator_list "," "}"
        { Enum($2, List.rev $4)}
    | ENUM "{" enumerator_list "," "}"
        { Enum("", List.rev $3)}
    | ENUM IDENT
        { Enum($2, [])}

enumerator_list:
    | enumerator
        { [$1] }
    | enumerator_list "," enumerator
        { $2 :: $1 }

enumerator:
    | IDENT
        { ($1, None) }
    | IDENT "=" constant_expression 
        { ($1, Some $3) }

type_qualifier:
    | CONST     { Const    }
    | VOLATILE  { Volatile }
    | RESTRICT  { Restrict }

function_specifier:
    | INLINE { Inline }

declarator:
    | direct_declarator
        { $1 }
    | pointer direct_declarator
        { DPointer($2) }

direct_declarator:
    | IDENT
        { DIdentifier $1}
    | "(" declarator ")"
    | direct_declarator "[" type_qualifier_list assignment_expression"]"
        { DArray($1, $3, DArraySize($4)) }
    | direct_declarator "[" assignment_expression "]"
        { DArray($1, [], DArraySize($4)) }
    | direct_declarator "[" type_qualifier_list "]"
        { DArray($1, $3, DArrayNone) }
    | direct_declarator "[" "]"
        { DArray($1, [], DArrayNone) }
    | direct_declarator "[" STATIC type_qualifier_list assignment_expression "]"
        { DArray($1, Static::$4, DArraySize($5)) }
    | direct_declarator "[" STATIC assignment_expression "]"
        { DArray($1, Static::[], DArraySize($5)) }
    | direct_declarator "[" type_qualifier_list STATIC assignment_expression "]"
        { DArray($1, $4 @ [Static], DArraySize($5)) }
    | direct_declarator "[" type_qualifier_list "*" "]"
        { DArray($1, $3, DArrayVLA) }
    | direct_declarator "[" "*" "]"
        { DArray($1, [], DArrayVLA) }
    | direct_declarator "(" parameter_type_list ")"
        { DFunction($1, $3) }
    | direct_declarator "(" identifier_list ")"
        { DFunction($1, $3) }
    | direct_declarator "(" ")"
        { DFunction($1, []) }

pointer:
    | "*" type_qualifier_list 
        { $2 }
    | "*" type_qualifier_list pointer
        { DPointerChain($2, $3) }

type_qualifier_list:
    | type_qualifier
        { $1::[] }
    | type_qualifier_list type_qualifier
        { $2::$1 }

parameters_type_list:
    | parameter_list
        { List.rev $1 }
    | parameter_list "," "..."
        { List.rev $1 }    

parameters_list:
    | parameters_declaration
        { $1 }
    | parameters_list "," parameters_declaration
        { $2 :: $1 }

parameters_declaration:
    | declaration_specifiers declarator
    | declaration_specifiers abstract_declarator
    | declaration_specifiers

identifier_list:
    | identifier
        { $1 }
    | identifier_list "," identifier
        { $1 :: $2 }

type_name:
    | specifier_qualifier_list abstract_declarator
        { DDeclaration ($)}
*/







statement:
    | labeled_statement
        { $1 }
    | compound_statement
        { $1 }
    | expression_statement
        { $1 }
    | selection_statement
        { $1 }
    | iteration_statement
        { $1 }
    | jump_statement
        { $1 }

labeled_statement:
    | IDENT ":" statement
        { Label($1, $3) }
    | CASE constant_expression ":" statement
        { Case ($2, $4) }
    | DEFAULT ":" statement
        { Default ($3)}

compound_statement:
    | "{" block_item_list "}"
        { Bloc (List.rev $2) }
    | "{}"
        { Bloc([]) }

block_item_list:
    | block_item
        { $1 :: [] }
    | block_item_list block_item
        { $2::$1 }

block_item:
    | declaration   { $1 }
    | statement   { $1 }

expression_statement:
    | expression ";"
        { $1 }
    | ";" 
        { Identifier("0pass")}

declaration:
    | INT {Break}

selection_statement:
    | IF "(" expression ")" statement
        { IfThenElse(If, $3, extract_bloc $5, []) }
    | IF "(" expression ")" statement ELSE statement
        { IfThenElse(If, $3, extract_bloc $5, extract_bloc $7) }
    | SWITCH "(" expression ")" statement
        { Switch($3, extract_bloc $5) }

opt_expression:
    | { None }
    | expression { Some $1}

iteration_statement:
    | WHILE "(" expression ")" statement
        { While(NoWhile, $3, extract_bloc $5)}
    | DO statement WHILE "(" expression ")" ";"
        { While(DoWhile, $5, extract_bloc $2)}
    | FOR "(" opt_expression ";" opt_expression ";" opt_expression ")" statement
        { For ($3, $5, $7, extract_bloc $9)}
    | FOR "(" declaration opt_expression ";" opt_expression ")" statement
        { For (Some $3, $4, $6, extract_bloc $8)}

jump_statement:
    | GOTO IDENT ";"
        { Goto($2) }
    | CONTINUE ";"
        { Continue }
    | BREAK ";"
        { Break }
    | RETURN ";"
        { Return (None) }
    | RETURN expression ";"
        { Return (Some $2)}



/*** external definitions ***/

translation_unit:
    | external_declaration
        { $1 }
    | translation_unit external_declaration
        { $2 :: $1 }

external_declaration:
    | function_definition
        { $1 }
    | declaration
        { $1 }

function_definition:
    | declaration_specifiers declarator declaration_list compournd_statement
        { Break }
    | declaration_specifiers declarator 
        { Break }

declaration_list:
    | declaration
        { $1 }
    | declaration_list declaration
        { $2 :: $1 }


/*** preprocessing ***/
