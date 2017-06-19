

type number_base = 
    | Hex
    | Dec
    | Oct


type exponent = 
    | Exponent of char * int * int
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
end


type type_name = Int


type conditionnal_type =
  | Ternary
  | If


type while_type =
  | DoWhile
  | NoWhile


type struct_declaration = 
  string * (declaration_specifiers list * ast list) list



and enumerator = 
  string * ast option

and array_size_d =
  | DArraySize of ast
  | DArrayNone
  | DArrayVLA
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

  | Struct of struct_declaration
  | Union of struct_declaration

  | Enum of string * enumerator list

  | Const
  | Volatile
  | Restrict

  | Inline

and declaration = 
  | DPointerChain of declaration_specifiers * declaration
  | DPointer of declaration
  | DIdentifier of string
  | DArray  of declaration * declaration_specifiers list * array_size_d

  | DDeclaration of declaration * ast option

and cast_argument = 
  | InitializerList of ast list
  | SeldomArg      of ast

and type_content = int

and designator = 
  | DesStruct of ast
  | DesMember of string

and ast = 
    | Identifier of string
    | Constant of constant
    | String of string
    | Call of ast * ast list
    | Access of access_method * ast * ast
    | UnaryOp of UnOp.op * ast
    | Cast  of type_name * cast_argument 
    | BinaryOp of BinOp.op * ast * ast
    | Assign of BinOp.op * ast * ast
    | Type of type_name
    | Expression of ast list
    | Declaration of  (string * declaration_specifiers list * ast option * (designator option * ast) list) list (* name, specifiers, things like pointer/array, value *)

    | IfThenElse of conditionnal_type * ast * ast list * ast list
    | Return of ast option
    | Break
    | Continue
    | Goto of string
    | For of ast option * ast option * ast option * ast list
    | Bloc of ast list
    | Switch of ast * ast list
    | While of while_type * ast * ast list
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
