open Ast
open Prettyprint

let read = 1 lsl 0
let write = 1 lsl 1
let readwrite = read lor write
let is_array = 1 lsl 2
let is_function = 1 lsl 3

let print_rw_flag f =
  let r = if f land read = read then "read " else ""
  in let w = if f land write = write then "write " else ""
  in let a = if f land is_array = is_array then "array" else ""
  in let u = if f land is_function = is_function then "function " else ""
  in r ^ w ^ a ^ u

let string_of_rw_flag f = 
  let r = if f land read = read then "r" else ""
  in let w = if f land write = write then "w" else ""
  in "f_" ^ r ^ w 

let openacc_dir_of_flag f =
  if f land (read lor write) = read lor write then
    "pcopy"
  else if f land read = read then
    "pcopyin"
  else 
    "pcopyout"

let is_array_flag f =
  f land is_array = is_array


let has_access variables name access = 
  if Hashtbl.mem variables name then
    let _, t = Hashtbl.find variables name in
    List.exists (fun (permissions, _, _, _) ->
      (permissions land access) = access
      )
      t
  else false

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
let uuid_iterateur = ref 0


let pretty_print_iterator it = 
  let (var_name, _, start, stop, (_, step)) = it
  in Printf.sprintf "(%s %s %s %s)" var_name (pretty_print_ast start) (pretty_print_ast stop) (pretty_print_ast step)


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
        | (p', f, i, u) :: tl when ((p' lor permission) land is_array != is_array) || (f = forloop && indices = i) ->
          (permission lor p', f, i, u @ uuids) :: tl
        | x :: tl ->
          x :: aux tl
      in Hashtbl.replace tbl name (l', aux p')
    else
      Hashtbl.add tbl name (level, [permission, forloop, indices, uuids])



  let rec aux tbl forloop_list indices_list uuids level permission program program_rewrote =
    let aux = aux tbl in
  match program, program_rewrote with
    | Identifier (";", _), _ | Identifier ("", _), _  -> ()
    | Identifier(s, uuid), _ -> 
      Printf.printf "=> %s : %d\n" s permission;
      add_variable tbl s forloop_list indices_list (uuid :: uuids) (-1) permission

    | Declaration (_, l), Declaration(_, l') ->
      List.iter2 (fun ((s, uuid), _, d, a) (_, _, _, a')->
          let f_a = match d with | DeArray _ -> is_array | DeFunction _ -> is_function | _ -> 0 in
          let _ = add_variable tbl s forloop_list indices_list (uuid :: uuids) level (permission lor f_a lor write)
          in match a, a' with | None, _ -> () | Some a, Some a' -> aux forloop_list indices_list uuids level permission a a' ) l l'

    | InitializerList l, InitializerList l' 
    | Expression l, Expression l' ->
      List.iter2 (aux forloop_list indices_list  uuids level permission) l l'

    | Call (w, l), Call(w', l') ->
      aux  forloop_list indices_list uuids level (permission lor read lor is_function) w w';
      List.iter2 (aux forloop_list indices_list uuids  level permission) l l'

    | Access (Array, a, b), Access(Array, a', b') ->  begin
        aux forloop_list (b'::indices_list) uuids level (permission lor is_array) a a';
        (*print_endline @@ "====> adding " ^ pretty_print_ast b ^ " with flah " ^ (print_rw_flag permission);*)
        aux forloop_list indices_list uuids  level ((permission lor read) land lnot write land lnot is_array) b b';
      end

    | UnaryOp(UnOp.PostDecr, a), UnaryOp(UnOp.PostDecr, a')
    | UnaryOp(UnOp.PreDecr, a), UnaryOp(UnOp.PreDecr, a')
    | UnaryOp(UnOp.PreIncr, a), UnaryOp(UnOp.PreIncr, a')
    | UnaryOp(UnOp.PostIncr, a), UnaryOp(UnOp.PostIncr, a') ->
      print_endline "ETZYERYERY";
      aux forloop_list indices_list  uuids level (permission lor read lor write) a a'

    | Access (_, a, _), Access(_, a', _) 
    | UnaryOp(_, a), UnaryOp(_, a')
    | Cast (_, a), Cast(_, a') ->
      aux forloop_list indices_list  uuids level (permission lor read) a a'

    | Default a, Default a'
    | Label(_, a), Label(_, a') ->
      aux forloop_list indices_list  uuids level permission a a'

    | BinaryOp(_, a, b), BinaryOp(_, a', b') -> 
      aux forloop_list indices_list  uuids level (permission lor read) a a';
      aux forloop_list indices_list  uuids level (permission lor read) b b';

    | Switch (a, b), Switch(a', b')
    | While (_, a, b), While(_, a', b')
    | Case (a, b), Case(a', b') -> 
      aux forloop_list indices_list  uuids level permission a a';
      aux forloop_list indices_list  uuids level permission b b';

    | Assign (_, a, b), Assign(_, a', b') ->
      aux forloop_list indices_list  uuids level (permission lor write) a a'; 
      aux forloop_list indices_list  uuids level (permission lor read) b b'


    | Return (Some a), Return(Some a') ->
      aux forloop_list indices_list  uuids level permission a a'

    | For (a, b, c, d), For (a', b', c', d') ->
      let f x x' = match x, x' with | None, _ -> () | Some x, Some x' -> 
        aux forloop_list indices_list  uuids (level + 1) permission x x' in
      f a a'; f b b'; f c c'; 
      begin try
          (* we create iterators from the expandend expression *)
          let i = create_iterateur (For(a', b', c', d'))
              (*TODO move content pure loop for index rewriting here *)
          in  aux (i::forloop_list) indices_list  uuids level permission d d'
        with Not_found ->
      aux forloop_list indices_list  uuids level permission d d'
          end

    | Bloc l, Bloc l' ->
      List.iter2 (aux forloop_list indices_list  uuids (level + 1) permission) l l'

    | FunctionDeclaration(_, (name, _, _), _, content), FunctionDeclaration (_, _, _, content') ->
      (* we should had the name, but laziness is the winner *)
      aux forloop_list indices_list  uuids level permission content content'
    | _ -> ()






(* return a triplet (i, start, end, step) if we have a pure for loop.
   i is an iterator going from start to end (included) by a step of step
   *)
and create_iterateur for_loop =
  match for_loop with
  | For(Some start, Some end_cond, Some it, stmts) ->
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
    in let temp = (var_name, !uuid_iterateur, start, stop, step)
    in let _ = incr uuid_iterateur
    in let _ = 
         (*let _ = print_endline "=============" in
         let _ = print_endline @@ pretty_print_ast stmts in *)
         (*let r = get_all_variables ([stmts]) in 
           if has_access r var_name write then
             let _ = print_endline "Not a pure loop"
             in raise Not_found
           else *)
           print_endline "Found for pure loop"
    in temp

  | _ -> raise Not_found



and get_all_variables program program_rewrote = 
  let tbl = Hashtbl.create 0 in
  let _ = aux tbl [] [] [] 0 0 program  program_rewrote
  in tbl



(* could do better and more lisible if we add ocaml 4.03.0
   unfortunately, on ubuntu ocaml 4.02.3 is more easily accessible *)
let filter_global_variables variables =
    let out = Hashtbl.create 0 in
    let _ = Hashtbl.iter 
        (fun name (level, p) ->
           if level = -1 then
             Hashtbl.add out name p
        ) variables
    in out
    


(* replace all identifier having an id in ids by expr 
   ast is the inputed ast
   ids the uuids of the identifier to replace
   expr a function which given an identifier, returns the
   sub-ast it will become
*)
let rec rename ast ids expr =
  let rec treat_opt = function
    | None -> None
    | Some a -> Some (aux a)
  and aux ast = match ast with
    | Identifier(_, id) ->
      if List.mem id ids then
        expr ast
      else 
        ast
    | BinaryOp(op, a, b) ->
      BinaryOp(op, aux a, aux b)
    | UnaryOp(op, a) ->
      UnaryOp(op, aux a)
    | InitializerList l ->
      InitializerList (List.map aux l)
    | Call(a, l) ->
      Call(aux a, List.map aux l)
    | Access(m, a, b) ->
      Access(m, aux a, aux b)
    | Cast (t, a) ->
      Cast (t, aux a)
    | Assign(op, a, b) ->
      Assign(op, aux a, aux b)
    | Expression l ->
      Expression (List.map aux l)
    | IfThenElse(c', a, b, c) ->
      IfThenElse(c', aux a, aux b, aux c)
    | Return a ->
      Return (treat_opt a)
    | For (a, b, c, d) ->
      For (treat_opt a, treat_opt b, treat_opt c, aux d)
    | Bloc l ->
      Bloc (List.map aux l)
    | Switch (a, b) ->
      Switch (aux a, aux b)
    | While (w, a, b) ->
      While (w, aux a, aux b)
    | Label(s, a) ->
      Label(s, aux a)
    | Case(a, b) ->
      Case (aux a, aux b)
    | Declaration(a, l) ->
      Declaration(a, List.map (fun (b, c, d, e) ->
        (b, c, d, treat_opt e)
        ) l)
    | Default(a) ->
      Default(aux a)
    | _ -> ast


  in aux ast
