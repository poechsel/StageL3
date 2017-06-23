open Ast
open Prettyprint


(* our constant foldings doesn't work for struct members *)

type constant_propagation =
  | CstConst of ast
  | CstNotConst
  | CstUnknown



module Env = Map.Make(String)

let fst_t (a, _, _) = a

let debug env = 
  Printf.printf "=====> START\n";
  Env.iter (fun k (id, deps, _) -> begin
      Printf.printf "%s: %d -> " k id;
      List.iter (fun (n, i) -> Printf.printf "%s:%d, " n i) deps;
      Printf.printf "\n" end
    ) env;
  Printf.printf "=====> END\n"

let rec is_expr_propagatable expr = 
  match expr with
  | Identifier _ | Constant _ -> true
  | BinaryOp(_, a, b) -> is_expr_propagatable a && is_expr_propagatable b
  | UnaryOp(op, a) -> begin
      match op with
      | UnOp.PreIncr | UnOp.PostIncr | UnOp.PreDecr | UnOp.PostDecr ->
        false
      | _ ->
        is_expr_propagatable a
    end
  | _ -> false

let get_all_dependencies expr =
  let rec aux expr l = 
    match expr with
    | Identifier s -> s::l
    | BinaryOp(_, a, b) -> let l' = aux a l in aux b l'
    | UnaryOp(_, a) -> aux a l
    | _ -> l
  in aux expr []


let rec expand_expr expr env = 
  debug env;
  let rec expand expr prev =
  match expr with
  | Identifier name ->
    if Env.mem name env then
      let _, deps, e = Env.find name env in
      let fct (dep, ver) = 
        if not (Env.mem dep env) then true 
        else ver > fst_t (Env.find dep env) 
      in if List.for_all fct deps && not (List.mem name prev) then
        expand e (name::prev)
      else expr
    else expr
  | BinaryOp(op, a, b) ->
    BinaryOp(op, expand a prev , expand b prev)
  | UnaryOp(op, a) ->
    UnaryOp(op, expand a prev)
  | _ -> expr

  in let expr = expand expr [] in expr


let constant_propagation expr =
  let time = ref 0 in
  let rec propagate affectation expr env = 
    let propagate_ = propagate affectation in
    let _ = incr time in
    match expr with
    | Identifier name ->
      expr, Env.add name (!time, [], expr) env
    | Assign(BinOp.Empty, Identifier(name), expr) when is_expr_propagatable expr ->
      let deps = get_all_dependencies expr in
      let deps = List.map
        (fun name ->
           name, fst_t (Env.find name env)
        )
        deps in
      let expr = expand_expr expr env in
      let env = Env.add name (!time, deps, expr) env in
      Assign(BinOp.Empty, Identifier(name), expr), env

    | Assign(op, Identifier(name), expr) ->
      propagate_ (Assign(BinOp.Empty, Identifier(name), BinaryOp(op, Identifier(name), expr))) env

    | Assign(op, a, expr) ->
      let a, env = propagate true a env in
      let expr, env = propagate_ expr env in
      Assign(op, a, expr), env

    | BinaryOp(op, a, b) -> 
      let a, env = propagate_ a env in
      let b, env = propagate_ b env in
      BinaryOp(op, a, b), env

    | UnaryOp(op, a) ->
      let a, env = propagate_ a env in
      UnaryOp(op, a), env

    | Bloc l ->
      let rec aux l env = 
        match l with
        | [] -> [], env
        | x::tl -> 
          print_endline @@ "\n====> analyzing: " ^ (pretty_print_ast x) ^ "";
          let x', env = propagate_ x env in
          let l', env' = aux tl env
          in x'::l', env'
      in let l, env = aux l env in Bloc l, env

    | Declaration(specs, l) -> 
      let rec aux l env = 
        match l with
        | [] -> [], env
        | (name, useless1, useless2, None)::tl -> 
          let l', env' = aux tl env
          in (name, useless1, useless2, None)::l', 
             Env.add name (!time, [], Identifier name) env'
        | (name, useless1, useless2, Some expr)::tl ->
          let deps = get_all_dependencies expr in
          let deps = List.map
              (fun name ->
                 name, if not(Env.mem name env) then 0 else fst_t (Env.find name env)
              )
              deps in
          let t = String.concat " " (List.map fst deps)
          in let _ = Printf.printf "adding %s with deps %s\n" name t in

          let l', env' = aux tl env
          in (name, useless1, useless2, Some expr)::l', 
             Env.add name (!time, deps, expr) env'

      in let l, env = aux l env
      in Declaration(specs, l), env
    | _ -> expr, env


  (*  | Identifier name when affectation = false -> begin
      let _ = print_endline "YES\n" in
      if Env.mem name env then
        let _, deps, e = Env.find name env in
        if (List.for_all (fun (dep, ver) -> if not (Env.mem dep env) then true else let _ = Printf.printf "dep is %s, wanting %d, have %d" dep ver (fst_t (Env.find dep env)) in ver >= fst_t (Env.find dep env)) deps)  
        
        
        then e, env
        else expr, env
      else expr, Env.add name (0, [], Identifier name) env
             end
    | Identifier name -> 
        expr, Env.add name (!time, [], Identifier name) env


    | Assign(BinOp.Empty, Identifier(name), expr) when is_expr_propagatable expr ->
      let expr, env = propagate_ expr env in
      let deps = get_all_dependencies expr in
      let deps = List.map
        (fun name ->
           name, fst_t (Env.find name env)
        )
        deps
      in Assign(BinOp.Empty, Identifier(name), expr), Env.add name (!time, deps, expr) env

    | Assign(op, a, expr) ->
      let _ = print_endline "################" in
      let a, env = propagate true a env in
      let _ = print_endline "################" in
      let expr, env = propagate_ expr env in
      Assign(op, a, expr), env

    | BinaryOp(op, a, b) -> 
      let a, env = propagate_ a env in
      let b, env = propagate_ b env in
      BinaryOp(op, a, b), env

    | UnaryOp(op, a) ->
      let a, env = propagate_ a env in
      UnaryOp(op, a), env

    | Bloc l ->
      let rec aux l env = 
        match l with
        | [] -> [], env
        | x::tl -> 
          let x', env = propagate_ x env in
          let l', env' = aux tl env
          in x'::l', env'
      in let l, env = aux l env in Bloc l, env

    | Declaration(specs, l) -> 
      let rec aux l env = 
        match l with
        | [] -> [], env
        | (name, useless1, useless2, None)::tl -> 
          let l', env' = aux tl env
          in (name, useless1, useless2, None)::l', 
             Env.add name (!time, [], Identifier name) env'
        | (name, useless1, useless2, Some expr)::tl ->
          let deps = get_all_dependencies expr in
          let deps = List.map
              (fun name ->
                 name, if not(Env.mem name env) then 0 else fst_t (Env.find name env)
              )
              deps in
          let t = String.concat " " (List.map fst deps)
          in let _ = Printf.printf "adding %s with deps %s\n" name t in
          let expr, env = propagate_ expr env in

          let l', env' = aux tl env
          in (name, useless1, useless2, Some expr)::l', 
             Env.add name (!time, deps, expr) env'

      in let l, env = aux l env
      in Declaration(specs, l), env



    | Constant c -> expr, env
*)
  in fst @@ propagate false expr (Env.empty)



(*
let rec is_constant_expr expr env = 
  match expr with
  | Identifier i -> 
    if Env.mem i env then (match Env.find i env with
      | CstNotConst -> false
      | _ -> true), env
    else let t = CstConst expr in true, Env.add i t env
  | Constant _ -> true, env
  | BinaryOp(_, a, b) -> 
    let s, e = is_constant_expr a env 
    in let s', e' = is_constant_expr b e
    in s && s', e'
  | UnaryOp(_, a) ->
    is_constant_expr a env
  | _ -> false, env



(* unifying two ast will probably always return false 

   We don't know constant folding for one reason: we can have float. 
   In this case, a+b = b+a is false sometimes, and we can't allow us to fall into this thing
   *)
let rec unify_ast a b =
  match (a, b) with
  | Identifier a, Identifier b -> a = b
  | Constant a, Constant b -> a = b
  | BinaryOp(op, a, b), BinaryOp(op', a', b') ->
    op = op' && unify_ast a a' && unify_ast b b'
  | UnaryOp(op, a), UnaryOp(op', a') ->
    op = op' && unify_ast a a' 
  | _ -> false

let unify a b = 
  match a, b with
  | CstUnknown, CstUnknown -> 
    true
  | CstNotConst, CstNotConst ->
    true
  | CstConst a, CstConst b ->
    unify_ast a b
  | _ ->
    false

(* remove keys who are in env_to_restrict and not in env_target *)
let restrict_env env_target env_to_restrict =
  let keys = Env.fold (fun k _ l -> k::l) env_to_restrict []
  in let rec aux l  out =
       match l with
       | [] -> out
       | k :: tl ->
         if Env.mem k env_target then
           aux tl out
         else 
           aux tl (Env.remove k out)
    in aux keys env_to_restrict


(* unify two env *)
let unify_env env env' =
  let keys = Env.fold (fun k v l -> (k, v)::l) env' []
  in let rec aux l env =
       match l with
       | [] -> env
       | (k, v)::tl -> 
         if unify v (Env.find k env) then
           aux tl env
        else 
          aux tl (Env.add k CstNotConst env)
  in aux keys env
       
*)
