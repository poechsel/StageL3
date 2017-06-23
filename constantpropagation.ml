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

  type t = ast Env.t

  let empty = Env.empty
  let update_version env variable =
              Env.add variable  (Identifier(variable)) env

  let get_last env name = 
    if not (Env.mem name env) then
      Identifier(name)
    else 
        Env.find name env

  let add_binding env name expr = 
    Env.add name  expr env


  (* remove vars that are in env' but not in env*)
  let restrict env' env = 
    Env.filter (fun key _ -> Env.mem key env) env'

  (* unify two environments *)
  let unify env env' = 
      Env.merge (fun key v v' ->
          match v, v' with
          | Some a, Some b when a = b -> Some a
          | _ -> None
        )
      env env'

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
  | Call(_, l) ->
    List.for_all is_expr_propagatable l
  | _ -> false

let rec expand_expr expr env =
    let _ = print_endline @@ pretty_print_ast expr in
  let rec expand expr = 
    match expr with
    | Identifier name -> 
      Env.get_last env name
    | UnaryOp(op, a) ->
      UnaryOp(op, expand expr)
    | BinaryOp(op, a, b) ->
      BinaryOp(op, expand a, expand b)
    | Access(t, what, where) ->
      Access(t, expand what, expand where)
    | Call(a, l) ->
      Call(a, List.map expand l)
    | _ -> expr

  in 
  if is_expr_propagatable expr then expand expr 
  else expr

let constant_propagation expr = 
  let env = Env.empty in

  let rec update_list l env = match l with
    | [] -> [], env
    | x::tl ->
      let _ = Printf.printf "iterating over %s\n" (pretty_print_ast x) in
      let x', env = propagate x env
      in let l, env = update_list tl env in  x'::l, env


  and transform expr env name = 
    if is_expr_propagatable expr then 
      let expr, env = propagate expr env in
      let env = Env.update_version env name in
      expr, Env.add_binding env name expr 
    else 
      expr, Env.update_version env name 

  and propagate expr env =
    match expr with
    | Identifier _ ->
      expand_expr expr env, env 
    | Access (a, what, where) ->
      let w, env = propagate where env in
      let what, env = propagate what env in
      Access(a, what, w), env

    

    | Assign(BinOp.Empty, Access(t, a, where), expr) ->
      (* do not enregister the modification in the env for these access 
         otherwise it will bug, like in A[iozer] = 897; int b= A;
         when registering, it becomes A[..] = 987; int b = 987*)
      let expr, env =  propagate expr env  in
      let where, env = propagate where env in
      let a, _ = propagate a env in 
      Assign(BinOp.Empty, Access(t, a, where), expr), env

    | Assign(BinOp.Empty, (Identifier(name) as a), expr) ->
      let expr, env = transform expr env name  in 
      Assign(BinOp.Empty, a, expr), env

    | Assign(op, (Access _ as a), expr) 
    | Assign(op, (Identifier _ as a), expr) ->
      propagate (Assign(BinOp.Empty, a, BinaryOp(op, a, expr))) env


    | UnaryOp(UnOp.PreIncr, Identifier name)
    | UnaryOp(UnOp.PreDecr, Identifier name) 
    | UnaryOp(UnOp.PostDecr, Identifier name) 
    | UnaryOp(UnOp.PostIncr, Identifier name) ->
      expr, Env.update_version env name

    | UnaryOp _ when is_expr_propagatable expr ->
      expand_expr expr env, env

   | UnaryOp(op, a) ->
      let  a, env = propagate a env in
        UnaryOp(op, a), env
   
        
    | BinaryOp _ when is_expr_propagatable expr ->
      expand_expr expr env, env
    | BinaryOp(op, a, b) ->
      let  a, env = propagate a env in
      let  b, env = propagate b env in
        BinaryOp(op, a, b), env

(*
    | Label(a, b) ->
      let b, env = propagate b env in
      Label(a, b), env
    | Case b ->
      let b, env = propagate b env in
      Case b, env
      *)
    | IfThenElse(a, cond, s_if, s_else) ->
      let cond, env = propagate cond env in
      let s_if, env' = propagate s_if env in
      let s_else, env'' = propagate s_else env in

      let env' = Env.restrict env' env in
      let env'' = Env.restrict env'' env in
      let env = Env.unify env' env'' in
      IfThenElse(a, cond, s_if, s_else), env

    | For(a, b, c, content) ->
      let aux env = function
        | None -> None, env
        | Some x -> let x, env = propagate x env in Some x, env
      in
      let a, env = aux env a in
      let b, env = aux env b in
      let c, env = aux env c in
      let content, env' = propagate content env in
      let env' = Env.restrict env' env in
      let env = Env.unify env env' in
      For(a, b, c, content), env

    | String _ ->
      expr, env

    | Switch (cond, content) ->
      let cond, env = propagate cond env in
      let content, env' = propagate content env in
      let env' = Env.restrict env' env in
      let env = Env.unify env env' in
      Switch (cond, content), env
(* 

    | UnaryOp(UnOp.PreIncr, a) 
    | UnaryOp(UnOp.PostIncr, a) ->
      propagate 
        (Assign(BinOp.Empty, a, 
                BinaryOp(BinOp.Add, a, 
                         Constant(CInt(Dec, Num.num_of_int 1, ""))
                        )
               )
        ) env

    | UnaryOp(UnOp.PreDecr, a) 
    | UnaryOp(UnOp.PostDecr, a) ->
      propagate 
        (Assign(BinOp.Empty, a, 
                BinaryOp(BinOp.Sub, a, 
                         Constant(CInt(Dec, Num.num_of_int 1, ""))
                        )
               )
        ) env


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
          let env = Env.update_version env name in
          let l, env = aux tl env in
          (name, spec, decl, None) :: l, env
        | (name, spec, decl, Some expr) :: tl ->
          let expr, env = transform expr env name in
          let l, env = aux tl env in 
          (name, spec, decl, Some expr) :: l, env
      in let l, env = aux l env
      in Declaration(spec, l), env

    | Constant _ ->
      expr, env

    | _ -> failwith (pretty_print_ast expr)

  in fst @@ propagate expr env
