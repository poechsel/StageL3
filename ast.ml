

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

type cast_argument = 
  | InitializerList of ast list
  | SeldomArg      of ast

and ast = 
    | Identifier of string
    | Constant of constant
    | String of string
    | Call of ast * ast list
    | Access of access_method * ast * ast
    | UnaryOp of UnOp.op * ast
    | Cast  of type_name * cast_argument 
    | BinaryOp of BinOp.op * ast * ast
    | IfThenElse of conditionnal_type * ast * ast * ast
    | Assign of BinOp.op * ast * ast
    | Type of type_name
    | Expression of ast list


let print_cst x = match x with
    | CInt(Hex, num, s) ->
        Printf.printf "(hex, %s, %s)" (Num.string_of_num num) s
    | CInt(Oct, num, s) ->
        Printf.printf "(oct, %s, %s)" (Num.string_of_num num) s
    | CInt(Dec, num, s) ->
        Printf.printf "(dec, %s, %s)" (Num.string_of_num num) s
    | _ -> ()
