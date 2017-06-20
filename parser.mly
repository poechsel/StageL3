%{
(* --- préambule: ici du code Caml --- *)
open Ast
open Num



let extract_bloc b = match b with
    | Bloc b -> b
    | b -> [b]

%}
/* description des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <string> IDENT STRING_LIT
%token <Ast.constant> CONSTANT
%token SIZEOF TYPEDEF EXTERN STATIC AUTO REGISTER VOID CHAR INT SHORT LONG FLOAT DOUBLE SIGNED UNSIGNED BOOL COMPLEX STRUCT UNION ENUM CONST VOLATILE RESTRICT INLINE
%token CASE SWITCH GOTO FOR WHILE IF ELSE BREAK CONTINUE RETURN DEFAULT DO


%token MULASSIGN DIVASSIGN MODASSIGN ADDASSIGN SUBASSIGN LSASSIGN RSASSIGN ANDASSIGN XORASSIGN ORASSIGN ASSIGN 
%token DOT RPAREN LPAREN RBRACKET LBRACKET RCURLY LCURLY INCR DECR ARROW MUL ANDBIN ORBIN XORBIN LEQ SLT SGT GEQ EQ NEQ DIV AND OR QUESTION THREEDOT COMA RSHIFT LSHIFT SUB NEG NOT ADD ENDLINE COLON MOD

%nonassoc below_ELSE
%nonassoc ELSE

%start main             
                       
%type <Ast.ast list> main

%%

main:                     
    | translation_unit ENDLINE
        { $1 }

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
    | LPAREN expression RPAREN
        { $2 }


postfix_expression:
    | primary_expression
        { $1 }
    | postfix_expression LBRACKET expression RBRACKET
        { Access(Array, $1, $3)}
    | postfix_expression LPAREN argument_expression_list RPAREN
        { Call($1, List.rev $3)}
    | postfix_expression LPAREN RPAREN
        { Call($1, [])}
    | postfix_expression DOT identifier
        { Access(Member, $1, $3)}
    | postfix_expression ARROW identifier
        { Access(Pointer, $1, $3)}
    | postfix_expression INCR 
        { UnaryOp(UnOp.PostIncr, $1)}
    | postfix_expression DECR 
        { UnaryOp(UnOp.PostDecr, $1)}
    
    | LPAREN type_name RPAREN LCURLY initializer_list RCURLY
        { Cast($2, InitializerList $5) }
    | LPAREN type_name RPAREN LCURLY initializer_list COMA RCURLY
        { Cast($2, InitializerList $5) }
     
argument_expression_list:
    | assignment_expression
        { [$1] }
    | argument_expression_list COMA assignment_expression
        { $3 :: $1 }

unary_operator:
    | MUL { UnOp.DeRef }
    | ANDBIN { UnOp.Ref }
    | NOT { UnOp.Not }
    | NEG { UnOp.Neg }
    | SUB { UnOp.Sub }
    | ADD { UnOp.Add }
unary_expression:
    | postfix_expression
        { $1 }
    | INCR unary_expression
        { UnaryOp(UnOp.PreIncr, $2) }
    | DECR unary_expression
        { UnaryOp(UnOp.PreDecr, $2) }
    | unary_operator unary_expression
        { UnaryOp($1, $2) }
    | SIZEOF LPAREN type_name RPAREN
        { UnaryOp(UnOp.SizeOf, Type $3) }
    | SIZEOF unary_expression
        { UnaryOp(UnOp.SizeOf, $2) }
 
cast_expression:
    | unary_expression
        { $1 }
    | LPAREN type_name RPAREN cast_expression
        { Cast($2, $4) }


multiplicative_expression:
    | cast_expression
        { $1 }
    | multiplicative_expression MUL cast_expression 
        { BinaryOp(BinOp.Mul, $1, $3) }
    | multiplicative_expression DIV cast_expression 
        { BinaryOp(BinOp.Div, $1, $3) }
    | multiplicative_expression MOD cast_expression 
        { BinaryOp(BinOp.Mod, $1, $3) }

additive_expression:
    | multiplicative_expression
        { $1 }
    | additive_expression SUB multiplicative_expression 
        { BinaryOp(BinOp.Sub, $1, $3) }
    | additive_expression ADD multiplicative_expression 
        { BinaryOp(BinOp.Add, $1, $3) }

shift_expression: 
    | additive_expression
        { $1 }
    | shift_expression LSHIFT additive_expression
        { BinaryOp(BinOp.LShift, $1, $3) }
    | shift_expression RSHIFT additive_expression
        { BinaryOp(BinOp.RShift, $1, $3) }

relational_expression: 
    | shift_expression
        { $1 }
    | relational_expression SLT shift_expression
        { BinaryOp(BinOp.Slt, $1, $3) }
    | relational_expression SGT shift_expression
        { BinaryOp(BinOp.Sgt, $1, $3) }
    | relational_expression LEQ shift_expression
        { BinaryOp(BinOp.Leq, $1, $3) }
    | relational_expression GEQ shift_expression
        { BinaryOp(BinOp.Geq, $1, $3) }


equality_expression:
    | relational_expression
        { $1 }
    | equality_expression EQ relational_expression
        { BinaryOp(BinOp.Eq, $1, $3) }
    | equality_expression NEQ relational_expression
        { BinaryOp(BinOp.Neq, $1, $3) }

and_expression:
    | equality_expression
        { $1 }
    | and_expression ANDBIN equality_expression
        { BinaryOp(BinOp.BinAnd, $1, $3)}
xor_expression:
    | and_expression
        { $1 }
    | xor_expression XORBIN and_expression
        { BinaryOp(BinOp.BinXor, $1, $3)}
or_expression:
    | xor_expression
        { $1 }
    | or_expression ORBIN xor_expression
        { BinaryOp(BinOp.BinOr, $1, $3)}
logical_and_expression:
    | or_expression
        { $1 }
    | logical_and_expression AND or_expression
        { BinaryOp(BinOp.And, $1, $3)}
logical_or_expression:
    | logical_and_expression
        { $1 }
    | logical_or_expression OR logical_and_expression
        { BinaryOp(BinOp.Or, $1, $3)}

conditional_expression:
    | logical_or_expression
        { $1 }
    | logical_or_expression QUESTION expression COLON conditional_expression
        { IfThenElse(Ternary, $1, [$3], [$5]) }

assignment_operator:
    | ASSIGN { BinOp.Empty }
    | MULASSIGN { BinOp.Mul }
    | DIVASSIGN { BinOp.Div }
    | MODASSIGN { BinOp.Mod }
    | ADDASSIGN { BinOp.Add }
    | SUBASSIGN { BinOp.Sub }
    | LSASSIGN { BinOp.LShift }
    | RSASSIGN { BinOp.RShift }
    | ANDASSIGN { BinOp.BinAnd }
    | XORASSIGN { BinOp.BinXor }
    | ORASSIGN { BinOp.BinOr }

assignment_expression:
    | conditional_expression
        { $1 }
    | unary_expression assignment_operator assignment_expression
        { Assign($2, $1, $3) }

/* must return it */
expression:
    | assignment_expression
        { $1 }
    | expression COMA assignment_expression
        { match $1 with
        | Expression l -> Expression ($3 :: l)
        | _ -> Expression($3 :: [$1])
        }


constant_expression:
    | conditional_expression
        { $1 }







/*** declarations ***/

declaration:
    | declaration_specifiers ENDLINE
        { Declaration($1, [])}
    | declaration_specifiers init_declarator_list ENDLINE
        { Declaration($1, List.rev $2)}

declaration_specifiers:
    | storage_class_specifier
        { $1 :: [] } 
    | type_specifier
        { $1 :: [] } 
    | function_specifier 
        { $1 :: [] } 
    | type_qualifier
        { $1 :: [] } 
    | storage_class_specifier declaration_specifiers
        { $1 :: $2 } 
    | type_specifier declaration_specifiers
        { $1 :: $2 } 
    | function_specifier declaration_specifiers
        { $1 :: $2 } 
    | type_qualifier declaration_specifiers
        { $1 :: $2 } 

init_declarator_list:
    | init_declarator
        { $1 :: [] }
    | init_declarator_list COMA init_declarator
        { $3 :: $1 }

init_declarator:
    | declarator
        { let a, b, v = $1 in a, b, v, None }
    | declarator ASSIGN initializer_
        { let a, b, v = $1 in a, b, v, Some $3 }

initializer_:
    | assignment_expression
        { $1 }
    | LCURLY initializer_list RCURLY
        { InitializerList $2 }
    | LCURLY initializer_list COMA RCURLY
        { InitializerList $2 }

initializer_list:
    | designation initializer_
        { Assign(BinOp.Empty, $1, $2) :: [] }
    | initializer_
        { $1 :: [] }
    | initializer_list COMA designation initializer_
        { Assign(BinOp.Empty, $3, $4) :: $1 }
    | initializer_list COMA designation 
        { $3 :: $1 }
        
designation :
    | designator_list ASSIGN
        { $1 }
designator_list:
    | designator
        { $1 }
    | designator_list designator
        { match $1 with
            | Access(method_, father, wut) ->
                    Access(method_, $2, wut) 
            | _ -> failwith "oups"
        }

designator:
    | LBRACKET constant_expression RBRACKET
        { Access(Array, Identifier(""), $2) }
    | DOT identifier
        { Access(Member, Identifier(""), $2) }

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

type_qualifier:
    | CONST     { Const    }
    | VOLATILE  { Volatile }
    | RESTRICT  { Restrict }

function_specifier:
    | INLINE { Inline }


struct_or_union_specifier:
    | STRUCT LCURLY struct_declaration_list RCURLY
        { Struct("", List.rev $3) }
    | STRUCT IDENT LCURLY struct_declaration_list RCURLY
        { Struct($2, List.rev $4) }
    | STRUCT IDENT
        { Struct ($2, []) }
    | UNION LCURLY struct_declaration_list RCURLY
        { Union("", List.rev $3) }
    | UNION IDENT LCURLY struct_declaration_list RCURLY
        { Union($2, List.rev $4) }
    | UNION IDENT
        { Union ($2, []) }

struct_declaration_list:
    | struct_declaration
        { $1 }
    | struct_declaration_list struct_declaration
        { $2 @ $1 }

struct_declaration:
    | specifier_qualifier_list struct_declarator_list ENDLINE
        {List.map (fun (a, b, c, d) -> $1, a, b, c, d) $2}

struct_declarator_list:
    | struct_declarator
        { $1 :: [] }
    | struct_declarator_list COMA struct_declarator
        { $3 :: $1 }

struct_declarator:
    | declarator
        { let a, b, c = $1 in a, b, c, None}
    | COLON constant_expression
        { "", [], DeBasic, Some $2 }
    | declarator COLON constant_expression
        { let a, b, c = $1 in a, b, c, Some $3 }

enum_specifier:
    | ENUM IDENT
        { Enum($2, []) }
    | ENUM IDENT LCURLY enumerator_list RCURLY 
        { Enum($2, List.rev $4)}
    | ENUM LCURLY enumerator_list RCURLY 
        { Enum("", List.rev $3)}
    | ENUM IDENT LCURLY enumerator_list COMA RCURLY 
        { Enum($2, List.rev $4)}
    | ENUM LCURLY enumerator_list COMA RCURLY 
        { Enum("", List.rev $3)}

enumerator_list:
    | enumerator
        { $1 :: [] }
    | enumerator_list COMA enumerator
        { $3 :: $1 }

enumerator:
    | IDENT
        { $1, None }
    | IDENT EQ constant_expression
        { $1, Some $3 }


declarator:
    | pointer direct_declarator
        { let id, b = $2 in id, $1, b }
    | direct_declarator
        { let id, b = $1 in id, [], b }

direct_declarator:
    | IDENT
        { $1, DeBasic }
    | LPAREN declarator RPAREN
        { let id, pr, de = $2 in id, DeRecursive(pr, de)}
    | direct_declarator LBRACKET type_qualifier_list assignment_expression RBRACKET
        { let id, de = $1 in id, DeArray(de, List.rev $3, DeArraySize $4)}
    | direct_declarator LBRACKET assignment_expression RBRACKET
        { let id, de = $1 in id, DeArray(de, [], DeArraySize $3)}
    | direct_declarator LBRACKET type_qualifier_list RBRACKET
        { let id, de = $1 in id, DeArray(de, List.rev $3, DeArrayUndef)}
    | direct_declarator LBRACKET RBRACKET
        { let id, de = $1 in id, DeArray(de, [], DeArrayUndef)}
    | direct_declarator LBRACKET STATIC assignment_expression RBRACKET
        { let id, de = $1 in id, DeArray(de, Static::[], DeArraySize $4)}
    | direct_declarator LBRACKET STATIC type_qualifier_list assignment_expression RBRACKET
        { let id, de = $1 in id, DeArray(de, Static::List.rev $4, DeArraySize $5)}
    | direct_declarator LBRACKET type_qualifier_list STATIC assignment_expression RBRACKET
        { let id, de = $1 in id, DeArray(de, List.rev (Static::$3), DeArraySize $5)}
    | direct_declarator LBRACKET type_qualifier_list MUL RBRACKET
        { let id, de = $1 in id, DeArray(de, List.rev $3, DeArrayVla)}
    | direct_declarator LBRACKET MUL RBRACKET
        { let id, de = $1 in id, DeArray(de, [], DeArrayVla)}
    | direct_declarator LPAREN parameter_type_list RPAREN
        { let id, de = $1 in id, DeFunction(de, List.rev $3) }
    | direct_declarator LPAREN identifier_list RPAREN
        { let id, de = $1 in id, DeFunction(de, List.rev $3) }
    | direct_declarator LPAREN RPAREN
        { let id, de = $1 in id, DeFunction(de, []) }

pointer:
    | MUL type_qualifier_list
        { Pointer :: $2 }
    | MUL 
        { Pointer :: [] }
    | MUL type_qualifier_list pointer
        { Pointer :: $2 @ $3 }
    | MUL pointer
        { Pointer :: $2}

type_qualifier_list:
    | type_qualifier
        { $1 :: [] }
    | type_qualifier_list type_qualifier
        { $2 :: $1 }

parameter_type_list:
    | parameter_list
        { $1 }
    | parameter_list COMA THREEDOT
        { DeOthers :: $1 }

parameter_list:
    | parameter_declaration
        { $1 :: [] }
    | parameter_list COMA parameter_declaration
        { $3 :: $1 }

parameter_declaration:
    | declaration_specifiers declarator
        { DeParam ($1, Some $2)}
    | declaration_specifiers 
        { DeParam ($1, None)}
    | declaration_specifiers abstract_declarator
        { DeParam ($1, Some $2)}

abstract_declarator:
    | pointer
        { "", $1, DeBasic }
    | pointer direct_abstract_declarator
        { let a, b = $2 in a, $1, b }
    | direct_abstract_declarator
        { let a, b = $1 in a, [], b }

direct_abstract_declarator:
    | LPAREN abstract_declarator RPAREN
        { let id, pr, de = $2 in id, DeRecursive(pr, de)}
    | LBRACKET type_qualifier_list assignment_expression RBRACKET
        { let id, de = "", DeBasic in id, DeArray(de, List.rev $2, DeArraySize $3)}
    | LBRACKET assignment_expression RBRACKET
        { let id, de = "", DeBasic in id, DeArray(de, [], DeArraySize $2)}
    | LBRACKET type_qualifier_list RBRACKET
        { let id, de = "", DeBasic in id, DeArray(de, List.rev $2, DeArrayUndef)}
    | LBRACKET RBRACKET
        { let id, de = "", DeBasic in id, DeArray(de, [], DeArrayUndef)}
    | LBRACKET STATIC assignment_expression RBRACKET
        { let id, de = "", DeBasic in id, DeArray(de, Static::[], DeArraySize $3)}
    | LBRACKET STATIC type_qualifier_list assignment_expression RBRACKET
        { let id, de = "", DeBasic in id, DeArray(de, Static::List.rev $3, DeArraySize $4)}
    | LBRACKET type_qualifier_list STATIC assignment_expression RBRACKET
        { let id, de = "", DeBasic in id, DeArray(de, List.rev (Static::$2), DeArraySize $4)}
    | LBRACKET MUL RBRACKET
        { let id, de = "", DeBasic in id, DeArray(de, [], DeArrayVla)}
    | LPAREN parameter_type_list RPAREN
        { let id, de = "", DeBasic in id, DeFunction(de, List.rev $2) }
    | LPAREN RPAREN
        { let id, de = "", DeBasic in id, DeFunction(de, []) }

    | direct_abstract_declarator LBRACKET type_qualifier_list assignment_expression RBRACKET
        { let id, de = $1 in id, DeArray(de, List.rev $3, DeArraySize $4)}
    | direct_abstract_declarator LBRACKET assignment_expression RBRACKET
        { let id, de = $1 in id, DeArray(de, [], DeArraySize $3)}
    | direct_abstract_declarator LBRACKET type_qualifier_list RBRACKET
        { let id, de = $1 in id, DeArray(de, List.rev $3, DeArrayUndef)}
    | direct_abstract_declarator LBRACKET RBRACKET
        { let id, de = $1 in id, DeArray(de, [], DeArrayUndef)}
    | direct_abstract_declarator LBRACKET STATIC assignment_expression RBRACKET
        { let id, de = $1 in id, DeArray(de, Static::[], DeArraySize $4)}
    | direct_abstract_declarator LBRACKET STATIC type_qualifier_list assignment_expression RBRACKET
        { let id, de = $1 in id, DeArray(de, Static::List.rev $4, DeArraySize $5)}
    | direct_abstract_declarator LBRACKET type_qualifier_list STATIC assignment_expression RBRACKET
        { let id, de = $1 in id, DeArray(de, List.rev (Static::$3), DeArraySize $5)}
    | direct_abstract_declarator LBRACKET MUL RBRACKET
        { let id, de = $1 in id, DeArray(de, [], DeArrayVla)}
    | direct_abstract_declarator LPAREN parameter_type_list RPAREN
        { let id, de = $1 in id, DeFunction(de, List.rev $3) }
    | direct_abstract_declarator LPAREN RPAREN
        { let id, de = $1 in id, DeFunction(de, []) }


identifier_list:
    | IDENT
        { DeIdentifier $1 :: [] }
    | identifier_list COMA IDENT
        { DeIdentifier $3 :: $1 }

specifier_qualifier_list:
    | type_specifier
        { $1 :: [] }
    | type_qualifier
        { $1 :: [] }
    | type_specifier specifier_qualifier_list
        { $1 :: $2 }
    | type_qualifier specifier_qualifier_list
        { $1 :: $2 }

type_name:
    | specifier_qualifier_list abstract_declarator
        { $1, $2 }
    | specifier_qualifier_list
        { $1, ("", [], DeBasic) }




/*** STATEMENTS ***/

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
    | IDENT COLON statement
        { Label($1, $3) }
    | CASE constant_expression COLON statement
        { Case ($2, $4) }
    | DEFAULT COLON statement
        { Default ($3)}

compound_statement:
    | LCURLY block_item_list RCURLY
        { Bloc (List.rev $2) }
    | LCURLY RCURLY
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
    | expression ENDLINE
        { $1 }
    | ENDLINE 
        { Identifier("0pass")}


selection_statement:
    | IF LPAREN expression RPAREN statement %prec below_ELSE
        { IfThenElse(If, $3, extract_bloc $5, []) }
    | IF LPAREN expression RPAREN statement ELSE statement 
        { IfThenElse(If, $3, extract_bloc $5, extract_bloc $7) }
    | SWITCH LPAREN expression RPAREN statement
        { Switch($3, extract_bloc $5) }

opt_expression:
    | { None }
    | expression { Some $1}

iteration_statement:
    | WHILE LPAREN expression RPAREN statement
        { While(NoWhile, $3, extract_bloc $5)}
    | DO statement WHILE LPAREN expression RPAREN ENDLINE
        { While(DoWhile, $5, extract_bloc $2)}
    | FOR LPAREN opt_expression ENDLINE opt_expression ENDLINE opt_expression RPAREN statement
        { For ($3, $5, $7, extract_bloc $9)}
    | FOR LPAREN declaration opt_expression ENDLINE opt_expression RPAREN statement
        { For (Some $3, $4, $6, extract_bloc $8)}

jump_statement:
    | GOTO IDENT ENDLINE
        { Goto($2) }
    | CONTINUE ENDLINE
        { Continue }
    | BREAK ENDLINE
        { Break }
    | RETURN ENDLINE
        { Return (None) }
    | RETURN expression ENDLINE
        { Return (Some $2)}



/*** external definitions ***/

translation_unit:
    | external_declaration
        { $1::[] }
    | translation_unit external_declaration
        { $2 :: $1 }

external_declaration:
    | function_definition
        { $1 }
    | declaration
        { $1 }

function_definition:
    | declaration_specifiers declarator  compound_statement
        { Break }
    | declaration_specifiers declarator declaration_list compound_statement
        { Break }

declaration_list:
    | declaration
        { $1::[] }
    | declaration_list declaration
        { $2 :: $1 }


/*** preprocessing ***/
