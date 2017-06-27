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


let binop_pure_for_loop binop i =
  match binop with
  | BinaryOp(op, Identifier(i, uuid), e)
  | BinaryOp(op, e, Identifier(i, uuid)) ->
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
  | Assign(op, Identifier(i, uuid), expr) ->
    begin
    match op with
    | BinOp.Add
    | BinOp.Sub
    | BinOp.Mul
    | BinOp.Div -> true
    | _ -> false
    end
  | UnaryOp(op, Identifier(i, uuid)) ->
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
      | Declaration(_, [(name, _), _, _, Some start]) -> name, start
      | Assign(BinOp.Empty, Identifier(name, uuid), start) -> name, start
      | _ -> failwith "start indices bad formatted"
    in let stop = match end_cond with
        | Identifier(x, uuid) when x = var_name -> Constant(CInt(Dec, Num.num_of_int 0, ""))
        (* i < n *)
        | BinaryOp(BinOp.Slt, Identifier(r, uuid), l) when r = var_name -> 
          BinaryOp (BinOp.Sub, l, one)
        (* n < i *)
        | BinaryOp(BinOp.Slt, r, Identifier(l, uuid)) when l = var_name -> 
          BinaryOp (BinOp.Add, r, one)
        (* i > n *)
        | BinaryOp(BinOp.Sgt, Identifier(r, uuid), l) when r = var_name -> 
          BinaryOp (BinOp.Add, l, one)
        (* n > i *)
        | BinaryOp(BinOp.Sgt, r, Identifier(l, uuid)) when l = var_name -> 
          BinaryOp (BinOp.Sub, r, one)
            (* <= and => *)
        | BinaryOp(BinOp.Leq, r, Identifier(l, uuid)) 
        | BinaryOp(BinOp.Leq, Identifier(l, uuid), r)  
        | BinaryOp(BinOp.Geq, r, Identifier(l, uuid))  
        | BinaryOp(BinOp.Geq, Identifier(l, uuid), r) when l = var_name -> 
          r
        | _ -> let _ = Printf.printf "========ERROR ===========\n%s\n" (pretty_print_ast end_cond) in failwith "not a good stop condition"

    in let step = match it with 
        | UnaryOp(UnOp.PostIncr, Identifier(x, uuid)) 
        | UnaryOp(UnOp.PreIncr, Identifier(x, uuid)) when x = var_name ->
          (BinOp.Add, one)
        | UnaryOp(UnOp.PostDecr, Identifier(x, uuid)) 
        | UnaryOp(UnOp.PreDecr, Identifier(x, uuid)) when x = var_name ->
          (BinOp.Sub, one)
        | Assign(op, Identifier(x, uuid), b) when x = var_name ->
          (op, b)
        | _ -> failwith "wrong step"
    in (var_name, start, stop, step)

  | _ -> raise Not_found


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


let add_variable tbl name forloop indices uuids level permission =
    if Hashtbl.mem tbl name then
      let l', p'= Hashtbl.find tbl name in
      let rec aux l = match l with
        | [] -> [(permission, forloop, indices, uuids)]
        | (p', f, i, u) :: tl when ((p' lor permission) land is_array != is_array) || (f == forloop && indices == i) ->
          (permission lor p', f, i, u @ uuids) :: tl
        | x :: tl ->
          x :: aux tl
      in Hashtbl.replace tbl name (l', aux p')
    else
      Hashtbl.add tbl name (level, [permission, forloop, indices, uuids])

let get_all_variables program = 
  let tbl = Hashtbl.create 0 in
  let rec aux forloop_list indices_list uuids level permission program =
  match program with
    | Identifier (";", _) | Identifier ("", _)  -> ()
    | Identifier(s, uuid) -> 
      print_string "ident\n";
      print_endline (pretty_print_ast program);
      print_endline "";
      add_variable tbl s forloop_list indices_list (uuid :: uuids) (-1) permission

    | Declaration (_, l) ->
      print_string "declaration\n";
      print_endline (pretty_print_ast program);
      print_endline "";
      List.iter (fun ((s, uuid), _, d, a) ->
          let f_a = match d with | DeArray _ -> is_array | DeFunction _ -> is_function | _ -> 0 in
          let _ = add_variable tbl s forloop_list indices_list (uuid :: uuids) level (permission lor f_a lor write)
          in match a with | None -> () | Some a -> aux forloop_list indices_list uuids level permission a  ) l

    | InitializerList l 
    | Expression l ->
      List.iter (aux forloop_list indices_list  uuids level permission) l

    | Call (w, l) ->
      aux  forloop_list indices_list uuids level (permission lor read lor is_function) w;
      List.iter (aux forloop_list indices_list uuids  level permission) l

    | Access (Array, a, b) ->  begin
        aux forloop_list (b::indices_list) uuids level (permission lor write lor is_array) a;
        aux forloop_list indices_list uuids  level ((permission lor read) land lnot is_array) b;
      end

    | UnaryOp(UnOp.PostDecr, a)
    | UnaryOp(UnOp.PreDecr, a)
    | UnaryOp(UnOp.PreIncr, a)
    | UnaryOp(UnOp.PostIncr, a) ->
      aux forloop_list indices_list  uuids level (permission lor read lor write) a

    | Access (_, a, _) 
    | UnaryOp(_, a) 
    | Cast (_, a) ->
      aux forloop_list indices_list  uuids level (permission lor read) a

    | Default a
    | Label(_, a) ->
      aux forloop_list indices_list  uuids level permission a

    | BinaryOp(_, a, b) -> 
      aux forloop_list indices_list  uuids level (permission lor read) a;
      aux forloop_list indices_list  uuids level (permission lor read) b;

    | Switch (a, b) 
    | While (_, a, b)
    | Case (a, b) -> 
      aux forloop_list indices_list  uuids level permission a;
      aux forloop_list indices_list  uuids level permission b;

    | Assign (_, a, b) ->
      aux forloop_list indices_list  uuids level (permission lor write) a; aux forloop_list indices_list  uuids level (permission lor read) b

    | IfThenElse (_, a, b, c) ->
      aux forloop_list indices_list  uuids level permission a;
      aux forloop_list indices_list uuids level permission b;
      aux forloop_list indices_list  uuids level permission c 

    | Return (Some a) ->
      aux forloop_list indices_list  uuids level permission a

    | For (a, b, c, d) ->
      let f x = match x with | None -> () | Some x -> 
        aux forloop_list indices_list  uuids (level + 1) permission x in
      f a; f b; f c; 
      begin try
          let i = create_iterateur (For(a, b, c, d))
          in  aux (i::forloop_list) indices_list  uuids level permission d
        with Not_found ->
      aux forloop_list indices_list  uuids level permission d
          end

    | Bloc l ->
      List.iter (aux forloop_list indices_list  uuids (level + 1) permission) l

    | FunctionDeclaration(_, (name, _, _), _, content) ->
      print_string "fuction\n";
      print_endline (pretty_print_ast program);
      print_endline "";
      (* we should had the name, but laziness is the winner *)
      aux forloop_list indices_list  uuids level permission content
    | _ -> ()



  in let _ = List.iter (fun x -> aux [] [] [] 0 0 x) program 
  in tbl
