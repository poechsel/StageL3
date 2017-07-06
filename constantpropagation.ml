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
  let update_version env variable uuid =
              Env.add variable  (Identifier(variable, uuid)) env

  let get_last env name uuid = 
    if not (Env.mem name env) then
      Identifier(name, uuid)
    else 
        Env.find name env

  let add_binding env name expr = 
    Env.add name  expr env

  let rec merge_postincr env postincrs = 
      match postincrs with
      | [] -> env
      | (x, which) :: l -> 
        let env = add_binding env x (BinaryOp(which, get_last env x (-1), Variables.one))
        in merge_postincr env l

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
  let rec expand expr = 
    match expr with
    | Identifier(name, uuid) -> 
      Env.get_last env name uuid
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
  let rec deal_with_list l env =
        match l with
        | [] -> [], env
        | x :: l ->
          let x', env' = propagate env x in
          let l', env' = deal_with_list l env' in
          (x'::l'), env'
  and propagate env expr =
    match expr with
    | Assign(BinOp.Empty, a, b) ->
      let b', env' = propagate env b in
      begin
        match a with
          | Identifier (name, _) ->
            let env' = Env.add_binding env' name b' in
            Assign(BinOp.Empty, a, b'), env'
          | _ -> 
            let a, env' = propagate env' a in
            let _ = Printf.printf "Didn't knew what to do with %s = ...\n" (pretty_print_ast a) in
            Assign(BinOp.Empty, a, b'), env'
            end
    
    | Identifier (name, uuid) ->
      Env.get_last env name uuid, env

    | Constant c ->
      Constant c, env

    | BinaryOp(op, a, b) ->
      let a, env = propagate env a in
      let b, env = propagate env b in
      let out = BinaryOp(op, a, b) in
      out, env

    | Call(what, l) ->
      let l, env = deal_with_list l env in
      Call (what, l), env
    | Bloc l ->
      let l, env = deal_with_list l env in
      Bloc l, env
    | InitializerList l ->
      let l, env = deal_with_list l env in
      InitializerList l, env
    | Expression l ->
      let l, env = deal_with_list l env in
      Expression l, env

    | Declaration (type_name, l) ->
      let rec analyse l env =
        match l with
        | [] -> [], env
        | ((name, uuid), declspec, decl, ast) :: l ->
          let ast, env = match ast with
            | None -> None, env
            | Some x ->
                  let x', env' = propagate env x in
                  let env' = Env.add_binding env' name x' in
                Some x', env'
                in
          let l', env' = analyse l env in
          ((name, uuid), declspec, decl, ast)::l', env'
      in let l, env = analyse l env in
      Declaration (type_name, l), env

      

    | UnaryOp (UnOp.PostIncr, a) ->
      begin
      match a with
      | Identifier (name, _) ->
        let a, env = propagate env a in
        let _ = print_endline @@ pretty_print_ast a in
        let a' = BinaryOp(BinOp.Add, a, Variables.one) in
        let env = Env.add_binding env name a' in
        a, env
      | _ ->
        failwith "didn't knew you could postincr dat"
          end
    | UnaryOp (UnOp.PostDecr, a) ->
      begin
      match a with
      | Identifier (name, _) ->
        let a, env = propagate env a in
        let a' = BinaryOp(BinOp.Sub, a, Variables.one) in
        let env = Env.add_binding env name a' in
        a, env
      | _ ->
        failwith "didn't knew you could postincr dat"
          end
    | UnaryOp (UnOp.PreIncr, a) ->
      begin
      match a with
      | Identifier (name, uuid) ->
        let a, env = propagate env a in
        let a = BinaryOp(BinOp.Add, a, Variables.one) in
        let env = Env.add_binding env name a in
        a, env
      | _ ->
        failwith "didn't knew you could preincr dat"
          end
    | UnaryOp (UnOp.PostDecr, a) ->
      begin
      match a with
      | Identifier (name, uuid) ->
        let a, env = propagate env a in
        let a = BinaryOp(BinOp.Sub, a, Variables.one) in
        let env = Env.add_binding env name a in
        a, env
      | _ ->
        failwith "didn't knew you could preincr dat"
          end

    | UnaryOp (op, a) ->
      let a, env = propagate env a in
      UnaryOp(op, a), env

    | Label(lbl, content) ->
      let content, env' = propagate env content in
      let env' = Env.restrict env' env in
      let env = Env.unify env' env in
        Label(lbl, content), env
    | Default content ->
      let content, env' = propagate env content in
      let env' = Env.restrict env' env in
      let env = Env.unify env' env in
        Default content, env

    | Cast (t, expr) ->
      let expr, env = propagate env expr in
      Cast (t, expr), env

    | Case(cond, content) ->
      let cond, env = propagate env cond in
      let content, env' = propagate env content in
      let env' = Env.restrict env' env in
      let env = Env.unify env' env in
        Case(cond, content), env

    | Return (Some a) ->
      let a, env = propagate env a in
      Return (Some a), env

    | Access(Array, what, where) ->
      let where, env = propagate env where in
      let what, env = propagate env what in
      Access(Array, what, where), env


    | IfThenElse(a, cond, s_if, s_else) ->
      let cond, env = propagate env cond in
      let s_if, env' = propagate env s_if in
      let s_else, env'' = propagate env s_else in

      let env' = Env.restrict env' env in
      let env'' = Env.restrict env'' env in
      let env = Env.unify env' env'' in
      IfThenElse(a, cond, s_if, s_else), env

    | For(a, b, c, content) ->
      let aux env = function
        | None -> None, env
        | Some x -> let x, env = propagate env x in Some x, env
      in
      (*let a, env = aux env a in
      let b, env = aux env b in
      let c, env = aux env c in
     *) let content, env' = propagate env content in
      let env' = Env.restrict env' env in
      let env = Env.unify env env' in
      For(a, b, c, content), env

    | Switch (cond, content) ->
      let cond, env = propagate env cond in
      let content, env' = propagate env content in
      let env' = Env.restrict env' env in
      let env = Env.unify env env' in
      Switch (cond, content), env

    | While(DoWhile, cond, content) ->
      let content, env' = propagate env content in
      let cond, env = propagate env cond in
      let env' = Env.restrict env' env in
      let env = Env.unify env env' in
      While(DoWhile, cond, content), env
    | While(NoWhile, cond, content) ->
      let content, env' = propagate env content in
      let cond, env' = propagate env' cond in
      let env' = Env.restrict env' env in
      let env = Env.unify env env' in
      While(NoWhile, cond, content), env



    | String s ->
      String s, env

    | expr ->
      expr, env

    



  in let a, _ = propagate env expr
  in a
