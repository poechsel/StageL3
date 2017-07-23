

type number_base = 
  | Hex
  | Dec
  | Oct


type exponent = 
  | Exponent of char * int * Num.num
type constant = 
  | CChar of string
  | CInt of  number_base * Num.num * string
  | CFloat of  number_base * Num.num * Num.num * exponent option * string (* base du flottant, entiere, decimal, exposant, flags*)

type access_method = 
  | Array
  | Member
  | Pointer

module UnOp = struct 
  type op =
    | PostIncr 
    | PostDecr
    | SizeOf
    | PreIncr
    | PreDecr
    | Ref     (* & *)
    | DeRef   (* * *)
    | Not     (* ! *)
    | Neg     (* ~ *)
    | Add
    | Sub
  let pretty_print str o = Printf.sprintf (
      match o with
      | PostIncr -> "%s ++"
      | PostDecr -> "%s --"
      | SizeOf  -> "sizeof(%s)" 
      | PreIncr -> "++ %s" 
      | PreDecr -> "-- %s"
      | Ref     -> "&%s"
      | DeRef   -> "*%s"
      | Not     -> "!%s"
      | Neg     -> "~%s"
      | Add     -> "+%s"
      | Sub     -> "-%s"
    ) str
end 

module BinOp = struct
  type op = 
    | Mul
    | Div
    | Mod
    | Add
    | Sub
    | LShift
    | RShift
    | Slt
    | Sgt
    | Leq
    | Geq
    | Eq
    | Neq
    | BinAnd
    | BinOr
    | BinXor
    | Or
    | And
    | Empty

  let invert o = 
    match o with
    | Leq -> Geq
    | Geq -> Leq
    | Slt -> Sgt
    | Sgt -> Slt
    | x -> x

  let negate o = match o with
    | Neq -> Eq
    | Eq -> Neq
    | Leq -> Sgt
    | Sgt -> Leq
    | Slt -> Geq
    | Geq -> Slt
    | x -> x

  let pretty_print o = match o with
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Add -> "+"
    | Sub -> "-"
    | LShift -> "<<"
    | RShift -> ">>"
    | Slt     -> "<"
    | Sgt     -> ">"
    | Leq     -> "<="
    | Geq     -> ">="
    | Eq      -> "=="
    | Neq     -> "!="
    | BinAnd  -> "&"
    | BinOr   -> "|"
    | BinXor  -> "^"
    | Or      -> "||"
    | And     -> "&&"
    | Empty   -> ""


  let is_op_comp op =
    match op with
    | Slt | Sgt | Leq | Geq | Neq | Eq -> true
    | _ -> false
end


(***
   For a struct, each entry is of the form:
   type, (name, list pointers & other sepcifiers, what (array, function qualifiers), bit size)
   For a normal declaration:
   (type, name, other sepcifiers, what, initialization)
 ***)

type conditionnal_type =
  | Ternary
  | If


type while_type =
  | DoWhile
  | NoWhile


type union_declaration = 
  string * (declaration_specifiers list ) list

and struct_declaration = 
  declaration_specifiers list * string * declaration_specifiers list * declarator * ast option


and enumerator = 
  string * ast option
and declaration_specifiers =
  | Typedef
  | Extern
  | Static
  | Auto
  | Register

  | Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Signed
  | Unsigned
  | Bool
  | Complex

  | Struct of string * struct_declaration list
  | Union of string * struct_declaration list

  | Enum of string * enumerator list

  | Const
  | Volatile
  | Restrict

  | Inline

  | Pointer

and declarator_array_size =
  | DeArraySize of ast
  | DeArrayVla 
  | DeArrayUndef

and type_name = 
  declaration_specifiers list * declarator_type

and declarator_type =
  (string * int) * declaration_specifiers list * declarator

and declarator_parameter =
  | DeIdentifier of string
  | DeOthers
  | DeParam of declaration_specifiers list * declarator_type option

and declarator = 
  | DeBasic
  | DeRecursive of declaration_specifiers list * declarator
  | DeArray of declarator * declaration_specifiers list * declarator_array_size
  | DeFunction of declarator * declarator_parameter list

and initializer_what =
  | InArray of ast
  | InMember of string

and ast = 
  | Identifier of string * int
  | InitializerList of ast list
  | Constant of constant
  | String of string
  | Call of ast * ast list
  | Access of access_method * ast * ast
  | UnaryOp of UnOp.op * ast
  | Cast  of type_name * ast 
  | BinaryOp of BinOp.op * ast * ast
  | Assign of BinOp.op * ast * ast
  | Type of type_name
  | Expression of ast list
  | Declaration of declaration_specifiers list * ((string * int) * declaration_specifiers list * declarator * ast option) list 
  | FunctionDeclaration of declaration_specifiers list * declarator_type * ast list * ast

  | IfThenElse of conditionnal_type * ast * ast * ast 
  | Return of ast option
  | Break
  | Continue
  | Goto of string
  | For of ast option * ast option * ast option * ast 
  | Bloc of ast list
  | Switch of ast * ast 
  | While of while_type * ast * ast 
  | Label of string * ast
  | Case of ast * ast
  | Default of ast


type preprocess =
  | PrInclude of string
  | PrDefine of string

type cod =
  | Ast of ast
  | Preprocess of preprocess


let print_cst x = match x with
  | CInt(Hex, num, s) ->
    Printf.printf "(hex, %s, %s)" (Num.string_of_num num) s
  | CInt(Oct, num, s) ->
    Printf.printf "(oct, %s, %s)" (Num.string_of_num num) s
  | CInt(Dec, num, s) ->
    Printf.printf "(dec, %s, %s)" (Num.string_of_num num) s
  | _ -> ()
