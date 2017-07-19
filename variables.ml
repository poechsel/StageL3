open Utils
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





(* for each indices appearing in a constraint, will create a copy of the new loop indices
   It is because iterators have local constraints (like in else / if)
   Also creates the new iterators
*)
let update_loop_indices loop_indices constraints =
  let constraints_name = List.map (fun (x, _, _, _) -> x) constraints
  in let constraints_name = List.sort_uniq Pervasives.compare constraints_name
  in let _ = Printf.printf "comparing " in
  let _ = List.iter (fun (x ) -> Printf.printf "%s " x) constraints_name in
  let _ = Printf.printf "  <=>  " in
  let _ = List.iter (fun (x, _, _) -> Printf.printf "%s " x) loop_indices in
  let _ = print_newline () in
  let loop_indices = List.map (fun (name, uuid, c) ->
      if List.mem name constraints_name then
        let _ = Printf.printf "updating it\n" in
        let _ = incr uuid_iterateur in
        (name, !uuid_iterateur, c)
      else (name, uuid, c)
    ) loop_indices
  in let to_add = List.fold_left ( fun prev name ->
      if not (List.exists (fun (x, _, _) -> x = name) loop_indices) then
        let _ = incr uuid_iterateur in
        let _ = Printf.printf "creating it\n" in
        (name, !uuid_iterateur, []) :: prev
      else prev
    ) [] constraints_name 
  in to_add @ loop_indices

let append_iterateur_constraints loop_indices constraints =
  List.map (fun (name, uuid, c) ->
      let temp = List.filter (fun (x, _, _, _) -> x = name) constraints
      in let temp = List.map (fun (_, a, b, c) -> (a, b, c)) temp
      in (name, uuid, temp @ c)

    ) loop_indices


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



let rec get_all_variables program program_rewrote = 
  let tbl = Hashtbl.create 0 in
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
      List.iter2 (aux forloop_list indices_list uuids level permission) l l'

    | Call (w, l), Call(w', l') ->
      aux  forloop_list indices_list uuids level (permission lor read lor is_function) w w';
      List.iter2 (aux forloop_list indices_list uuids  level permission) l l'

    | Access (Array, a, b), Access(Array, a', b') ->  begin
        aux forloop_list (b'::indices_list) uuids level (permission lor is_array) a a';
        (*print_endline @@ "====> adding " ^ pretty_print_ast b ^ " with flah " ^ (print_rw_flag permission);*)
        aux forloop_list indices_list uuids level ((permission lor read) land lnot write land lnot is_array) b b';
      end

    | UnaryOp(UnOp.PostDecr, a), UnaryOp(UnOp.PostDecr, a')
    | UnaryOp(UnOp.PreDecr, a), UnaryOp(UnOp.PreDecr, a')
    | UnaryOp(UnOp.PreIncr, a), UnaryOp(UnOp.PreIncr, a')
    | UnaryOp(UnOp.PostIncr, a), UnaryOp(UnOp.PostIncr, a') ->
      print_endline "ETZYERYERY";
      aux forloop_list indices_list uuids level (permission lor read lor write) a a'

    | Access (_, a, _), Access(_, a', _) 
    | UnaryOp(_, a), UnaryOp(_, a')
    | Cast (_, a), Cast(_, a') ->
      aux forloop_list indices_list uuids level (permission lor read) a a'

    | Default a, Default a'
    | Label(_, a), Label(_, a') ->
      aux forloop_list indices_list uuids level permission a a'

    | BinaryOp(_, a, b), BinaryOp(_, a', b') -> 
      aux forloop_list indices_list uuids level (permission lor read) a a';
      aux forloop_list indices_list uuids level (permission lor read) b b';

    | Switch (a, b), Switch(a', b')
    | While (_, a, b), While(_, a', b')
    | Case (a, b), Case(a', b') -> 
      aux forloop_list indices_list uuids level permission a a';
      aux forloop_list indices_list uuids level permission b b';

    | Assign (_, a, b), Assign(_, a', b') ->
      aux forloop_list indices_list uuids level (permission lor write) a a'; 
      aux forloop_list indices_list uuids level (permission lor read) b b'


    | Return (Some a), Return(Some a') ->
      aux forloop_list indices_list uuids level permission a a'

    | For (a, b, c, d), For (a', b', c', d') ->
      let f x x' = match x, x' with | None, _ -> () | Some x, Some x' -> 
        aux forloop_list indices_list uuids (level + 1) permission x x' in
      f a a'; f b b'; f c c'; 
      begin try
          (* we create iterators from the expandend expression *)
          let it_name, constraints' = create_iterateur (For(a', b', c', d')) forloop_list
            in let forloop_list = update_loop_indices forloop_list constraints'
            in let forloop_list = append_iterateur_constraints forloop_list constraints'
            (*TODO move content pure loop for index rewriting here *)
            in  aux forloop_list indices_list uuids level permission d d'


        with Not_found ->
          aux forloop_list indices_list uuids level permission d d'
      end

    | Bloc l, Bloc l' ->
      List.iter2 (aux forloop_list indices_list uuids (level + 1) permission) l l'

    | FunctionDeclaration(_, (name, _, _), _, content), FunctionDeclaration (_, _, _, content') ->
      (* we should had the name, but laziness is the winner *)
      aux forloop_list indices_list uuids level permission content content'
    | IfThenElse(_, cond, if_clause, else_clause), IfThenElse(_, cond', if_clause', else_clause') ->
      aux forloop_list indices_list uuids level permission cond cond';
      aux forloop_list indices_list uuids level permission if_clause if_clause';
      aux forloop_list indices_list uuids level permission else_clause else_clause'
    | _ -> ()

  in let _ = aux tbl  [] [] [] 0 0 program  program_rewrote
  in tbl


(* a constraint is a triplet
   (operator, right side, dividor)
   right side and dividor are in expression format
*)





(* return a triplet (i, start, end, step) if we have a pure for loop.
   i is an iterator going from start to end (included) by a step of step
*)
and create_iterateur for_loop indices =
  match for_loop with
  | For(Some start, Some end_cond, Some it, stmts) ->
    let var_name, indices, it = match start with
      | Declaration(_, [(name, _), _, _, Some start])  
      | Assign(BinOp.Empty, Identifier(name, _), start) -> 

        let indices = name :: List.map (fun (x, _, _) -> x) indices in
        let indices = unique_list indices in
        let temp = Calcul.operate start indices in
        let cons = (name, BinOp.Eq, temp, [Calcul.LC(Constant(CInt(Hex, Num.num_of_int (-1), "")))])
        in name, indices, cons
      | _ -> failwith "start indices bad formatted"
    in let constraints = Calcul.ineq_normalisation_constraint end_cond indices
    in let constraints = List.fold_left
           ( fun o (op, ineq) -> (Calcul.generate_constraints (op, ineq)) @ o)
           []
           constraints
    in let temp = var_name, it :: constraints
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
