open Ast
open Utils


(* here, our goal, is knowing we have an arithmetic expression, 
   to test if it is in the form a * i + b and retrieve a and b

   We will use this steps:
   - first, expand the expression in order to obtain something of the form
    a*b*c+d*e*f+g*h+...
   - secondly, sort the products terms
    We want reserved variables (ie loops indices) first
   - secondly, propagate the '-':
        -a*-b <=> a*b
        -a * b <=> -b
    Because the expression is sorted, the minus will go on a constant
   - Then, group the parts corresponding to an indices
*)


let is_mul_class op = 
  match op with 
  | BinOp.Mul (*| BinOp.Div*) -> true 
  | _ -> false

let is_add_class op =
  match op with 
  | BinOp.Add | BinOp.Sub -> true 
  | _ -> false


exception IncorrectExpr of string



(* we define a new type, which will be a bit easier to work with *)
type arithmetics =
  | LAdd of arithmetics list
  | LMul of arithmetics list
  | LUnop of UnOp.op * arithmetics
  | LC of ast




(*two auxiliary functions allowing us to check if an identifier is present inside an expression *)
let rec expr_find expr w = 
  match expr with
  | Identifier (name, _) -> name = w
  | BinaryOp(_, a, b) -> expr_find a w || expr_find b w
  | UnaryOp(_, a) -> expr_find a w
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




(* apply some bits of transformation to the provided ast.
   We check if a reserved variable doesn't exists in a denominator
   And if a reservesd variable isn't affected to a non monotonous function
   (like a binary and)
*)
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


(* expend an expression. It is easier to do it when the expression is made of binary operators *)
let rec expand expr = match expr with
  | BinaryOp(op1, BinaryOp(op2, a, b), BinaryOp(op3, c, d)) 
    when is_mul_class op1 && is_add_class op2 && is_add_class op3 ->

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

  | BinaryOp(op1, BinaryOp(op2, a, b), c) 
    when (is_mul_class op1 || BinOp.Div == op1) && is_add_class op2 ->

    let a = expand a in
    let b = expand b in
    let c = expand c in 
    expand @@ BinaryOp(op2,
                       BinaryOp(op1, a, c),
                       BinaryOp(op1, b, c)
                      )

  | BinaryOp(op1, c, BinaryOp(op2, a, b)) 
    when is_mul_class op1 && is_add_class op2 ->
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


(* negate a whole expression *)
let rec export_minus expr =
  match expr with
  | LC x -> LUnop(UnOp.Sub, expr)
  | LUnop(UnOp.Sub, a) -> a
  | LMul l -> LMul(LUnop(UnOp.Sub, List.hd l) :: List.tl l)
  | LAdd l -> LAdd (List.map export_minus l)
  | expr -> expr


(* convert an ast to a new arithms form *)
let rec convert_ast_to_arithms expr reserved =
  let expr = prepare_conversion expr reserved in
  let rec convert_ast_to_arithms expr =
    match expr with
    | BinaryOp(op, a, b) when is_add_class op ->
      let a = convert_ast_to_arithms a in
      let b = convert_ast_to_arithms b in
      let b = if op = BinOp.Sub then export_minus b else b in
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


(* pretty print our expression *)
let rec pretty_print_arithm expr = 
  (
    match expr with
    | LAdd l -> Prettyprint.__print_list pretty_print_arithm " + " l
    | LMul l -> Prettyprint.__print_list pretty_print_arithm " * " l
    | LUnop (op, a) ->
      UnOp.pretty_print (pretty_print_arithm a) op
    | LC a ->
      Prettyprint.pretty_print_ast a
  )


(* go through an unop chain to get the last element *)
let rec is_unop_chain expr = 
  match expr with
  | LC(Identifier (a, _)) -> Some a
  | LUnop (op, expr) -> is_unop_chain expr
  | _ -> None


(* compare two arithmetics expressions *)
let compare a b reserved =
  match (a, b) with
  | LUnop(op, expr), b ->
    let temp = is_unop_chain expr in
    begin match temp with 
      | Some a when List.mem a reserved -> -1
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


(* reorient to put the reserved name near the front *)
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
    | LUnop(UnOp.Sub, a) :: [] -> [a; LUnop(UnOp.Sub, LC(one))]
    | x :: l -> x :: aux l
  in
  match expr with
  | LMul l ->
    LMul (List.map move_unop_sub (aux l))
  | LAdd l ->
    LAdd (List.map move_unop_sub  l)
  | LUnop(UnOp.Sub, a) ->
    LMul ([a; LUnop(UnOp.Sub, LC(one))])
  | LUnop(op, a) ->
    LUnop(op, move_unop_sub a)
  | e -> e






(* remove an identifier from a product:
   a*b*c*i*d -> a*b*c*d *)
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


(* add a product of term to the list *)
let add_hashmp tbl name x =
  let x = if x = LMul([]) then LMul([LC(one)]) else x in
  if Hashtbl.mem tbl name then
    Hashtbl.replace tbl name (x :: Hashtbl.find tbl name)
  else Hashtbl.add tbl name [x]


(* finally get all the coefficients *)
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



(* check if an expression is in the form a + i b 
the expression is given in the form of operate
*)
let is_expr_abi_form expression =
  let offset = if Hashtbl.mem expression "" then 1 else 0 in
  Hashtbl.length expression <= 1 + offset




(* finally, do the magic *)
let operate expr reserved =
  let expr = expand expr in
  let expr = convert_ast_to_arithms expr reserved in
  let expr = reorient expr reserved in
  let expr = move_unop_sub expr  in
  let results = get_coefficients expr reserved in
  results



let rec ineq_normalisation_constraint expr reserved =
  match expr with
  | BinaryOp(BinOp.Or, expr1, expr2) 
  | BinaryOp(BinOp.And, expr1, expr2) ->
    ineq_normalisation_constraint expr1 reserved @ ineq_normalisation_constraint expr2 reserved
  | BinaryOp(op, expr1, expr2) when BinOp.is_op_comp op ->
    [(op, operate (BinaryOp(BinOp.Sub, expr2, expr1)) reserved)]

  | _ -> failwith "unknown operator"


let generate_constraints (op, ineq)  =
  let constraints = Hashtbl.create 0 in
  let append_constraint key content =
    if Hashtbl.mem constraints key then
      Hashtbl.replace constraints key (content :: Hashtbl.find constraints key)
    else
      Hashtbl.add constraints key [content]

  in let _ = Hashtbl.iter (
      fun key content ->
        if key = "" || content == [] then ()
        else 
          let ineq' = Hashtbl.copy ineq 
          in let _ = Hashtbl.remove ineq' key
          in let _ = append_constraint key (op, ineq', content)
(*          in let str = Hashtbl.fold (fun key content b ->
              b ^ "+" ^ key ^ "*" ^ __print_list Calcul.pretty_print_arithm "+" content
            ) ineq ""
          in let _ = print_endline @@
               "-(" ^key ^ "*" ^ __print_list Calcul.pretty_print_arithm "+" content ^ ")"
               ^ BinOp.pretty_print op ^ str
  *)      
          in ()
    ) ineq
in constraints

