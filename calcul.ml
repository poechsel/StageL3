open Ast
open Variables


let is_mul_class op = match op with | BinOp.Mul (*| BinOp.Div*) -> true | _ -> false
let is_add_class op = match op with | BinOp.Add | BinOp.Sub -> true | _ -> false

exception IncorrectExpr of string

let rec expr_find expr w = 
  match expr with
  | Identifier (name, _) -> name = w
  | BinaryOp(_, a, b) -> expr_find a w || expr_find b w
  | UnaryOp(_, a) -> expr_find a w
  | _ -> false

let rec prepare_conversion expr reserved = 
  match expr with
  | BinaryOp(BinOp.Div, a, b) ->
    let a = prepare_conversion a reserved in
    let b = prepare_conversion b reserved in
    if List.exists (fun x -> expr_find b x) reserved then
      raise (IncorrectExpr "a reserved variable can't appear in a denominator")
    else 
      BinaryOp(BinOp.Mul, a, BinaryOp(BinOp.Div, one, b))
  | BinaryOp(op, a, b) ->
    BinaryOp(op, prepare_conversion a reserved, prepare_conversion b reserved)
  | UnaryOp(op, (Identifier(a, _) as id)) when List.mem a reserved ->
    if op = UnOp.Sub then UnaryOp(op, id)
    else raise (IncorrectExpr "a reserved name can't have a unary operator other than minus")
  | UnaryOp(op, a) ->
    UnaryOp(op, prepare_conversion a reserved)
  | _ -> expr

let rec expand expr = match expr with
  | BinaryOp(op1, BinaryOp(op2, a, b), BinaryOp(op3, c, d)) when is_mul_class op1 && is_add_class op2 && is_add_class op3 ->
    let a = expand a in
    let b = expand b in
    let c = expand c in 
    let d = expand d in
    expand @@ BinaryOp(op3, 
             BinaryOp(op2,
                      BinaryOp(op1, a, c),
                      BinaryOp(op1, b, c)),
             BinaryOp(op2,
                      BinaryOp(op1, a, d),
                      BinaryOp(op1, b, d))
            )
  | BinaryOp(op1, BinaryOp(op2, a, b), c) when (is_mul_class op1 || BinOp.Div == op1) && is_add_class op2 ->
    let a = expand a in
    let b = expand b in
    let c = expand c in 
    expand @@ BinaryOp(op2,
             BinaryOp(op1, a, c),
             BinaryOp(op1, b, c)
            )
  | BinaryOp(op1, c, BinaryOp(op2, a, b)) when is_mul_class op1 && is_add_class op2 ->
    let a = expand a in
    let b = expand b in
    let c = expand c in 
    expand @@ BinaryOp(op2,
             BinaryOp(op1, c, a),
             BinaryOp(op1, c, b)
            )
  | BinaryOp(op, a, b) ->
    BinaryOp(op, expand a, expand b)
  | UnaryOp(op, a) -> 
    UnaryOp(op, expand a)
  | expr -> expr


type arithmetics =
  | LAdd of arithmetics list
  | LMul of arithmetics list
  | LUnop of UnOp.op * arithmetics
  | LC of ast


let rec export_minus expr =
  match expr with
  | LC x -> LUnop(UnOp.Sub, expr)
  | LUnop(UnOp.Sub, a) -> a
  | LMul l -> LMul(LUnop(UnOp.Sub, List.hd l) :: List.tl l)
  | LAdd l -> LAdd (List.map export_minus l)

let rec convert_ast_to_arithms expr reserved =
  let expr = prepare_conversion expr reserved in
let rec convert_ast_to_arithms expr =
  match expr with
  | BinaryOp(BinOp.Add, a, b) ->
    let a = convert_ast_to_arithms a in
    let b = convert_ast_to_arithms b in
    begin match a, b with
      | LAdd l, LAdd l' -> LAdd (l @ l')
      | b, LAdd l | LAdd l, b -> LAdd (b :: l)
      | a, b -> LAdd [a; b]
    end
  | BinaryOp(BinOp.Sub, a, b) ->
    let a = convert_ast_to_arithms a in
    let b = convert_ast_to_arithms b in
    let b = export_minus b in
    begin match a, b with
      | LAdd l, LAdd l' -> LAdd (l @ l')
      | b, LAdd l | LAdd l, b -> LAdd (b :: l)
      | a, b -> LAdd [a; b]
    end
  | BinaryOp(BinOp.Mul, a, b) ->
    let a = convert_ast_to_arithms a in
    let b = convert_ast_to_arithms b in
    begin match a, b with
      | LMul l, LMul l' -> LMul (l @ l')
      | b, LMul l | LMul l, b -> LMul (b :: l)
      | a, b -> LMul [a; b]
    end
  | UnaryOp(op, a) ->
    LUnop(op, convert_ast_to_arithms a)
  | _ -> LC expr
in convert_ast_to_arithms expr


let rec pretty_print_arithm expr = 
  "(" ^(
  match expr with
  | LAdd l -> Prettyprint.__print_list pretty_print_arithm " + " l
  | LMul l -> Prettyprint.__print_list pretty_print_arithm " * " l
  | LUnop (op, a) ->
    "OP" ^ UnOp.pretty_print (pretty_print_arithm a) op
  | LC a ->
    Prettyprint.pretty_print_ast a
)^ ")"


let rec is_unop_chain expr = 
  match expr with
  | LC(Identifier (a, _)) -> print_endline "zerze"; Some a
  | LUnop (op, expr) -> is_unop_chain expr
  | _ -> None

let compare a b reserved =
  let _ = Printf.printf "%s <-> %s\n" (pretty_print_arithm a) (pretty_print_arithm b) in
  match (a, b) with
  | LUnop(op, expr), b ->
    let temp = is_unop_chain expr in
    begin match temp with 
      | Some a when List.mem a reserved -> print_string "yes"; -1
      | _ -> 0
        end
  | a, LUnop(op, expr) ->
    let temp = is_unop_chain expr in
    begin match temp with 
      | Some a when List.mem a reserved -> 1
      | _ -> 0
        end
  | LC(Identifier(a, _)), LC(Identifier(b, _)) when List.mem a reserved && List.mem b reserved -> Pervasives.compare a b
  | LC(Identifier (a, _)), b when List.mem a reserved -> -1
  | a, LC(Identifier (b, _)) when List.mem b reserved -> 1
  | _ -> 0

let rec reorient expr reserved =
  let aux x = reorient x reserved in 
  let compare a b = compare a b reserved in
  match expr with
  | LAdd l ->
    let l = List.map aux l in
    LAdd (List.sort compare l)
  | LMul l ->
    let l = List.map aux l in
    LMul (List.sort compare l)
  | LUnop (op, a) -> LUnop(op, aux a)
  | _ -> expr


let rec move_unop_sub expr =
  let rec aux l = match l with
    | [] -> []
    | LUnop(UnOp.Sub, LUnop(UnOp.Sub, a)) :: l -> aux (a :: l)
    | LUnop(UnOp.Sub, a) :: n :: l -> a :: aux (LUnop(UnOp.Sub, n) :: l)
    | LUnop(UnOp.Sub, a) :: [] -> [a; LUnop(UnOp.Sub, LC(Variables.one))]
    | x :: l -> x :: aux l
  in
  match expr with
  | LMul l ->
    LMul (List.map move_unop_sub (aux l))
  | LAdd l ->
    LAdd (List.map move_unop_sub  l)
  | LUnop(op, a) ->
    LUnop(op, move_unop_sub a)
  | e -> e



let rec reorient_ids expr loops_ids = 
  match expr with
  | BinaryOp(op, (Identifier (a, _) as a'), b) when List.mem a loops_ids ->
    BinaryOp(op, reorient_ids b loops_ids, a')
  | BinaryOp(op, a, b) ->
    BinaryOp(op, reorient_ids a loops_ids, reorient_ids b loops_ids)
  | UnaryOp(op, a) -> 
    UnaryOp(op, reorient_ids a loops_ids)
  | expr -> expr

let rec check_constants expr constants = 
  match expr with
  | Identifier (name, _) ->
    List.mem name constants
  | Constant _ -> true
  | BinaryOp(op, a, b) ->
    check_constants a constants && check_constants b constants
  | UnaryOp(op, a) ->
    check_constants a constants
  | _ -> false


let rec arithm_find expr w =
  let aux x = arithm_find x w in
  match expr with
  | LMul l
  | LAdd l ->
    List.exists aux l
  | LC(Identifier(name, _)) -> name = w
  | LC _ -> false
  | LUnop(op, x) -> aux x


let remove_ident_from_expr expr w = 
  let rec aux expr status =
  match expr with
  | [] -> []
  | LC(Identifier (name, _))::tl when name = w ->
    if status = true then
      raise (IncorrectExpr "we can only deal with first degree polynomials for the moment")
    else 
      aux tl true
  | x :: tl -> 
    x :: aux tl status
  in aux expr false

    
let add_hashmp tbl name x =
  if Hashtbl.mem tbl name then
    Hashtbl.replace tbl name (x :: Hashtbl.find tbl name)
  else Hashtbl.add tbl name [x]


let get_coefficients expr reserved =
  let expr = match expr with
  | LAdd l -> 
    List.map (fun x -> match x with | LMul y -> y | y -> [y]) l
  | LMul l -> [l]
  | _ -> [[expr]]
  in
  let tbl = Hashtbl.create 0 in
  let _ = List.iter (fun x -> Hashtbl.add tbl x []) reserved in
  let _ = List.iter (
    fun x ->
      let test = ref false in 
      let _ = List.iter (fun name ->
          if arithm_find (LMul(x)) name then
            let _ = test := true in
            add_hashmp tbl name (LMul (remove_ident_from_expr x name))
        ) reserved
      in if !test = false then
        add_hashmp tbl "" (LMul x)
      else ()
  ) expr
in tbl

    
