open Ast
open Prettyprint


(* our constant foldings doesn't work for struct members *)

type constant_propagation =
  | CstConst of ast
  | CstNotConst
  | CstUnknown



let key_to_string name ver = name ^ ";" ^ string_of_int ver

module Env = struct
  module Env = Map.Make (String)

  type t = {versions : int Env.t ; bindings :  ast Env.t}

  let empty = {versions = Env.empty; bindings = Env.empty}
  let update_version env variable version =
    {env with versions = Env.add variable version env.versions}

  let get_last env name = 
    if not (Env.mem name env.versions) then
      Identifier(name)
    else 
      let last_version = Env.find name env.versions in
      let key = key_to_string name last_version in
      if Env.mem key env.bindings then
        Env.find key env.bindings
      else Identifier(name)

  let add_binding env name expr = 
    let version = Env.find name env.versions in
    {env with bindings = Env.add (key_to_string name version) expr env.bindings}

end

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
  | Access(_, what, where) ->
    is_expr_propagatable what && is_expr_propagatable where
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
  let rec expand expr = 
    let _ = print_endline @@ pretty_print_ast expr in
    match expr with
    | Identifier name -> 
      Env.get_last env name
    | UnaryOp(op, a) ->
      UnaryOp(op, expand expr)
    | BinaryOp(op, a, b) ->
      BinaryOp(op, expand a, expand b)
    | Access(t, what, where) ->
      Access(t, what, expand where)
    | _ -> expr

  in 
  if is_expr_propagatable expr then expand expr 
  else expr

let transform expr env name time = 
  if is_expr_propagatable expr then 
    let expr = expand_expr expr env in
    let env = Env.update_version env name !time in
    expr, Env.add_binding env name expr 
  else 
    expr, Env.update_version env name !time 

let constant_propagation expr = 
  let env = Env.empty in
  let time = ref 0 in

  let rec update_list l env = match l with
    | [] -> [], env
    | x::tl ->
      let x', env = propagate x env
      in let l, env = update_list tl env in  x'::l, env
  and propagate expr env =
    let _ = incr time in
    let _ = Printf.printf "ver : %d\n" !time in 
    match expr with
    | Identifier name -> 
      expr, Env.update_version env name !time

    | Assign(BinOp.Empty, Access(t, (Identifier(name) as a), where), expr) ->
      (* do not enregister the modification in the env for these access 
         otherwise it will bug, like in A[iozer] = 897; int b= A;
         when registering, it becomes A[..] = 987; int b = 987*)
      let expr, _ =  transform expr env name time in
      let where = expand_expr where env in
      Assign(BinOp.Empty, Access(t, a, where), expr), env

    | Assign(BinOp.Empty, (Identifier(name) as a), expr) ->
      let expr, env = transform expr env name time in
      Assign(BinOp.Empty, a, expr), env

    | Assign(op, (Access _ as a), expr) 
    | Assign(op, (Identifier _ as a), expr) ->
      propagate (Assign(BinOp.Empty, a, BinaryOp(op, a, expr))) env

(*
    | Label(a, b) ->
      let b, env = propagate b env in
      Label(a, b), env
    | Case b ->
      let b, env = propagate b env in
      Case b, env
      *)


    | Bloc l ->
      let l, env = update_list l env
      in Bloc l, env
    | Expression l ->
      let l, env = update_list l env
      in Expression l, env
    | Call (a, l) ->
      let l, env = update_list l env
      in Call(a, l), env

    | Declaration(spec, l) ->
      let rec aux l env = match l with
        | [] -> l, env
        | (name, spec, decl, None)::tl ->
          let env = Env.update_version env name !time in
          let l, env = aux tl env in 
          (name, spec, decl, None) :: l, env
        | (name, spec, decl, Some expr) :: tl ->
          let expr, env = transform expr env name time in
          let l, env = aux tl env in 
          (name, spec, decl, Some expr) :: l, env
      in let l, env = aux l env
      in Declaration(spec, l), env

    | Constant _ ->
      expr, env

    | _ -> failwith (pretty_print_ast expr)


  in fst @@ propagate expr env


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
