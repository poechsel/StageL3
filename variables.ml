open Ast
open Prettyprint

let read = 1 lsl 0
let write = 1 lsl 1
let is_array = 1 lsl 2
let is_function = 1 lsl 3

let print_rw_flag f =
  let r = if f land read = read then "read " else ""
  in let w = if f land write = write then "write " else ""
  in let a = if f land is_array = is_array then "array" else ""
  in let u = if f land is_function = is_function then "function " else ""
  in r ^ w ^ a ^ u
      

let get_all_variables program =
  let tbl = Hashtbl.create 0 in
  let rec aux level permission program = 
  match program with
    | Identifier ";" | Identifier ""-> ()
    | Identifier s -> 
      print_string "ident\n";
      print_endline (pretty_print_ast program);
      print_endline "";
      if Hashtbl.mem tbl s then 
              let l', p' = Hashtbl.find tbl s in
 Hashtbl.replace tbl s (l', permission lor p' )

      else Hashtbl.add tbl s (-1, permission)

    | Declaration (_, l) ->
      print_string "declaration\n";
      print_endline (pretty_print_ast program);
      print_endline "";
      List.iter (fun (s, _, d, a) ->
          let f_a = match d with | DeArray _ -> is_array | DeFunction _ -> is_function | _ -> 0 in
          let _ = if Hashtbl.mem tbl s then 
              let l', p' = Hashtbl.find tbl s in
              Hashtbl.replace tbl s (level, permission lor f_a lor p' lor write)
            else Hashtbl.add tbl s (level, permission lor write lor f_a)
          in match a with | None -> () | Some a -> aux level permission a  ) l

    | InitializerList l 
    | Expression l ->
      List.iter (aux level permission) l

    | Call (w, l) ->
      aux level (permission lor read lor is_function) w;
      List.iter (aux level permission) l

    | Access (Array, a, b) ->  begin
        aux level (permission lor write lor is_array) a;
        aux level (permission lor read) b;
      end

    | UnaryOp(UnOp.PostDecr, a)
    | UnaryOp(UnOp.PreDecr, a)
    | UnaryOp(UnOp.PreIncr, a)
    | UnaryOp(UnOp.PostIncr, a) ->
      aux level (permission lor read lor write) a

    | Access (_, a, _) 
    | UnaryOp(_, a) 
    | Cast (_, a) ->
      aux level (permission lor read) a

    | Default a
    | Label(_, a) ->
      aux level permission a

    | BinaryOp(_, a, b) -> 
      aux level (permission lor read) a;
      aux level (permission lor read) b;

    | Switch (a, b) 
    | While (_, a, b)
    | Case (a, b) -> 
      aux level permission a;
      aux level permission b;

    | Assign (_, a, b) ->
      aux level (permission lor write) a; aux level (permission lor read) b

    | IfThenElse (_, a, b, c) ->
      aux level permission a;
      aux level permission b;
      aux level permission c 

    | Return (Some a) ->
      aux level permission a

    | For (a, b, c, d) ->
      let f x = match x with | None -> () | Some x -> aux (level + 1) permission x in
      f a; f b; f c; aux level permission d

    | Bloc l ->
      List.iter (aux (level + 1) permission) l

    | FunctionDeclaration(_, (name, _, _), _, content) ->
      print_string "fuction\n";
      print_endline (pretty_print_ast program);
      print_endline "";
      (* we should had the name, but laziness is the winner *)
      aux level permission content
    | _ -> ()



  in let _ = List.iter (fun x -> aux 0 0 x) program 
  in tbl


let binop_pure_for_loop binop i =
  match binop with
  | BinaryOp(op, Identifier(i), e)
  | BinaryOp(op, e, Identifier(i)) ->
    begin
      match op with
      | BinOp.Slt
      | BinOp.Sgt
      | BinOp.Geq
      | BinOp.Leq
      | BinOp.Eq
      | BinOp.Neq -> true
      | _ -> false
    end
  | _ -> false

let unop_pure_for_loop op i = 
  match op with
  | Assign(op, Identifier(i), expr) ->
    begin
    match op with
    | BinOp.Add
    | BinOp.Sub
    | BinOp.Mul
    | BinOp.Div -> true
    | _ -> false
    end
  | UnaryOp(op, Identifier(i)) ->
    begin
      match op with
      | UnOp.PostIncr | UnOp.PostDecr | UnOp.PreIncr | UnOp.PreDecr -> true
      | _ -> false
    end
  | _ -> false

let one = Constant(CInt(Dec, Num.num_of_int 1, ""))

(* return a triplet (i, start, end, step) if we have a pure for loop.
   i is an iterator going from start to end (included) by a step of step
   *)
let create_iterateur for_loop =
  match for_loop with
  | For(Some start, Some end_cond, Some it, _) ->
    let var_name, start = match start with
      | Declaration(_, [name, _, _, Some start]) -> name, start
      | Assign(BinOp.Empty, Identifier name, start) -> name, start
      | _ -> failwith "start indices bad formatted"
    in let stop = match end_cond with
        | Identifier x when x = var_name -> Constant(CInt(Dec, Num.num_of_int 0, ""))
        (* i < n *)
        | BinaryOp(BinOp.Slt, Identifier r, l) when r = var_name -> 
          BinaryOp (BinOp.Sub, l, one)
        (* n < i *)
        | BinaryOp(BinOp.Slt, r, Identifier l) when l = var_name -> 
          BinaryOp (BinOp.Add, r, one)
        (* i > n *)
        | BinaryOp(BinOp.Sgt, Identifier r, l) when r = var_name -> 
          BinaryOp (BinOp.Add, l, one)
        (* n > i *)
        | BinaryOp(BinOp.Sgt, r, Identifier l) when l = var_name -> 
          BinaryOp (BinOp.Sub, r, one)
            (* <= and => *)
        | BinaryOp(BinOp.Leq, r, Identifier l) 
        | BinaryOp(BinOp.Leq, Identifier l, r)  
        | BinaryOp(BinOp.Geq, r, Identifier l)  
        | BinaryOp(BinOp.Geq, Identifier l, r) when l = var_name -> 
          r

    in let step = match it with 
        | UnaryOp(UnOp.PostIncr, Identifier x) 
        | UnaryOp(UnOp.PreIncr, Identifier x) when x = var_name ->
          (BinOp.Add, one)
        | UnaryOp(UnOp.PostDecr, Identifier x) 
        | UnaryOp(UnOp.PreDecr, Identifier x) when x = var_name ->
          (BinOp.Sub, one)
        | Assign(op, Identifier x, b) when x = var_name ->
          (op, b)
        | _ -> failwith "wrong step"
    in (var_name, start, stop, step)

  | _ -> failwith "not a pure for loop"


let rec detect_pure_for_loop program =
  let rec aux program = 
  match program with 
    | While (_, _, c) 
    | Switch (_, c) 
    | Case (_, c)
    | FunctionDeclaration (_, _, _, c) 
    | Label (_, c) ->
      aux c
    | IfThenElse (_, _, a, b) ->
        aux a;
        aux b;
    | For (Some(Declaration(_, [name, _, _, Some start_value])), Some (binop), Some (unop), content) 
      when unop_pure_for_loop unop name && binop_pure_for_loop binop name
      ->
      print_endline "found";
      aux content
    | Bloc l ->
      List.iter aux l
    | _ -> ()
  in List.iter aux program
