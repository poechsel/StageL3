open Ast
open Variables


let is_mul_class op = match op with | BinOp.Mul | BinOp.Div -> true | _ -> false
let is_add_class op = match op with | BinOp.Add | BinOp.Sub -> true | _ -> false


let rec pure_expr expr = match expr with
  | Constant _ -> true
  | Identifier _ -> true
  | BinaryOp(op, a, b) when is_mul_class op || is_add_class op -> 
    pure_expr a && pure_expr b
  | UnaryOp(UnOp.Sub, a) | UnaryOp(UnOp.Add, a) -> pure_expr a
  | _ -> false

let rec expand expr = match expr with
  | BinaryOp(op1, BinaryOp(op2, a, b), BinaryOp(op3, c, d)) when is_mul_class op1 && is_add_class op2 && is_add_class op3 ->
    BinaryOp(op3, 
             BinaryOp(op2,
                      BinaryOp(op1, a, c),
                      BinaryOp(op1, b, c)),
             BinaryOp(op2,
                      BinaryOp(op1, a, d),
                      BinaryOp(op1, b, d))
            )
  | BinaryOp(op1, BinaryOp(op2, a, b), c) when is_mul_class op1 && is_add_class op2 ->
    BinaryOp(op2,
             BinaryOp(op1, a, c),
             BinaryOp(op1, b, c)
            )
  | BinaryOp(op1, c, BinaryOp(op2, a, b)) when is_mul_class op1 && is_add_class op2 ->
    BinaryOp(op2,
             BinaryOp(op1, c, a),
             BinaryOp(op1, c, b)
            )
  | BinaryOp(op, a, b) ->
    BinaryOp(op, expand a, expand b)
  | UnaryOp(op, a) -> 
    UnaryOp(op, expand a)
  | expr -> expr

let rec present_id expr i = match expr with
  | Identifier j -> i = j
  | BinaryOp(_, a, b) -> present_id a i || present_id b i
  | UnaryOp(_, a) -> present_id a i
  | _ -> false

let not_present_id expr i = not (present_id expr i)

(*
let rec reorient_ids expr loops_ids constant_ids = 
  match expr with
  | BinaryOp(op, Identifier a, Identifier b)
    *)
