open Ast
open Prettyprint

let structures = "
typedef struct {
    int incr;
  int start;
  int end;
} iterator;

"


let rec expr_get_id_list expr = 
  match expr with
  | Identifier (name, _) -> [name]
  | BinaryOp(_, a, b) ->
    expr_get_id_list a @ expr_get_id_list b
  | Constant _ -> []
  | UnaryOp (_, a) ->
    expr_get_id_list a



let iterators_compare (_, a, _, _, _) (_, b, _, _, _) =
  let _ = Printf.printf "%d\n%d\n" a b in
  if a < b then -1
  else if a = b then 0
  else 1


(* return a list of iterators from the variables *)
let get_iterators_from_variables variables = 
  let a = Hashtbl.fold 
    (fun name (_, p) prev ->
        prev @ (List.fold_left
                  (fun a (_, i, _, _) ->
                     a @ i) 
                  [] 
                  p)
    )
    variables
    []
  in List.sort_uniq iterators_compare a





let build_corresponding_map its =
  let tbl = Hashtbl.create 0 in
  let _ = List.iter (fun (name, uuid, _, _, _) ->
    Hashtbl.add tbl name uuid
    ) its in
  tbl

let rec replace_expression_it expr its_map array_name =
  match expr with
  | Identifier(name, _) ->
    if Hashtbl.mem its_map name then
      Access(Array, Identifier(array_name, 0), Constant(CInt(Dec, Num.num_of_int (Hashtbl.find its_map name), "")))
    else expr
  | BinaryOp(op, a, b) ->
    BinaryOp(op, replace_expression_it a its_map array_name, 
            replace_expression_it b its_map array_name)
  | UnaryOp(op, a) ->
    UnaryOp(op, replace_expression_it a its_map array_name)
  | _ ->
    expr


(* fct expr return true if the expr is an identifier *)
let rec return_identifier_count fct expr =
  let rec aux expr i = 
    if fct expr then i+1
    else match expr with
      | BinaryOp (_, a, b) ->
        aux b (aux a i)
      | UnaryOp (_, a) ->
        aux a i
      | _ -> i
  in aux expr 0

(* the function change_op will change an expression *)
let change_id fct change_op target expr =
  let rec aux expr i = 
    if fct expr then 
      let expr = if i = target then
          change_op expr
        else expr
      in expr, i+1
    else match expr with
      | BinaryOp (op, a, b) ->
        let a, i = aux a i in
        let b, i = aux b i in
        BinaryOp(op, a, b), i
      | UnaryOp (op, a) ->
        let a, i = aux a i in
        UnaryOp(op, a), i
      | _ -> expr, i
  in fst (aux expr 0)

let change_all fct change_op expr =
  let rec aux expr = 
    if fct expr then 
          change_op expr
    else match expr with
      | BinaryOp (op, a, b) ->
        BinaryOp(op, aux a, aux b)
      | UnaryOp (op, a) ->
        UnaryOp(op, aux a)
      | _ -> expr
  in aux expr

let rec generate_all_perm array_name expr = 
  let fct = 
    (fun x -> match x with 
       | Access(Array, Identifier(a, _), _) when a = array_name -> true
       | _ -> false)
  in 
  let nb = return_identifier_count fct expr in
  let t = Array.make nb 0 in
  let expr = change_all
    fct
      (fun x -> Access(Member, x, Identifier("start", -1)))
      expr
  in 
  let rec aux expr i =
    if i >= nb then 
      [expr]
    else
      aux expr (i+1) @ 
      aux (change_id 
    (fun x -> match x with 
       | Access(Member, Access(Array, Identifier(a, _), _), _) when a = array_name -> true
       | _ -> false)
    (fun x -> match x with 
       | Access(Member, x, Identifier("start", -1)) -> Access(Member, x, Identifier("stop", -1))
       | _ -> x)
    i
      expr
    ) (i+1)
  in aux expr 0

let generate_reduction_in_c expr indices_map name_ar target fct_name =
  let expr = replace_expression_it expr indices_map name_ar in
  let exprs = generate_all_perm name_ar expr in
  let out = (List.fold_left (fun a b ->
      a ^
      target ^ " = " ^ fct_name ^"("^ target ^ ", " ^ (pretty_print_ast @@ b) ^ ");\n")
      ""
      exprs)
in
  print_string out



(* convert a expression in  the form returned by operator (list of arithm) 
   and return a tuple (min, max) 
    restricted is a map indices -> expression_min, expression_max


*)

let hashtbl_keys tbl = 
  let l = Hashtbl.fold (fun key _ p -> key :: p) tbl []
  in List.sort_uniq Pervasives.compare l


let expression_to_c expression restricted =
  let l, l' = Hashtbl.fold (
    fun name l (expr_m, expr_M)  ->
      if l = [] then (expr_m, expr_M)
      else
        let a = __print_list Calcul.pretty_print_arithm "+" l in
        let mi, ma = 
          if name = "" then (a, a)
          else 
            let l = a ^ "*" ^ fst @@ Hashtbl.find restricted name in
            let h = a ^ "*" ^ snd @@ Hashtbl.find restricted name in
            let mi = "min(" ^ l ^ ", " ^ h ^ ")" in
            let ma = "max(" ^ l ^ ", " ^ h ^ ")" in
            mi, ma
        in
        (mi::expr_m, ma::expr_M)
  ) expression ([], [])
  in __print_list (fun x -> x) " + " l, __print_list (fun x -> x) " + " l'


let create_iterators_in_c variables =
  let iterators = get_iterators_from_variables variables in
  let cm = build_corresponding_map iterators in
  let its = Hashtbl.create (List.length iterators) in
  let _ = List.iter 
      (fun (name, uuid, _, _, _) ->
         let base = "it_list[" ^ string_of_int uuid ^ "]" in
         Hashtbl.add its name (base ^ ".min", base ^ ".max")
      ) iterators in
  let its_list =  hashtbl_keys its in
  let _ = Printf.printf "s_iterators it_list[%d];\n" (Hashtbl.length cm) in
  let _ = List.iter (fun ((name, uuid, start, stop, _)) ->
      let start = Calcul.operate start its_list in
      let stop = Calcul.operate stop its_list in
      let target = "it_list[" ^ string_of_int uuid ^ "]" in
      let _ = Printf.printf "it_list[%d].min = %s;\n" uuid @@ fst @@ expression_to_c start its in
      Printf.printf "it_list[%d].max = %s;\n" uuid @@ snd @@ expression_to_c stop its 
    ) iterators
  in ()



let replace_iterators expr it_list = 
  let rec aux expr =
    match expr with
    | Access(Array, a, w) ->
      Access(Array, aux a, aux w)
    | Identifier (name, _) ->
      if List.exists (fun (n, _, _, _, _) -> n = name) it_list then
        let _, uuid, _, _, _ = List.find (fun (n, _,_, _, _) -> n = name) it_list in
        Access(Array, Identifier("it_list", 0), Constant(CInt(Dec, Num.num_of_int uuid, "")))
      else expr
    | BinaryOp(op, a, b) ->
      BinaryOp(op, aux a, aux b)
    | Call(w, l) -> 
      Call(w, List.map aux l)
    | UnaryOp(op, a) ->
      UnaryOp(op, aux a)
    | _ -> expr
  in aux expr


let transform_code_par ast variables =
  let ast = ref ast in
  let _ = Hashtbl.iter 
    (fun name (level, p) -> match level with
       | -1 -> List.iter
                 (fun (permissions, _, _, uuids) ->
                    if permissions land Variables.is_function = Variables.is_function then
                      ()
                    else 
                      let _ = print_endline @@ "seeing " ^ name  ^ " id = " ^ (__print_list string_of_int "," uuids) in
                      if permissions land Variables.is_array = Variables.is_array then
                      ast := Variables.rename !ast uuids (function
                          | Identifier(name, u) ->Access(Member, Identifier("s_"^name, u), Identifier(Variables.string_of_rw_flag permissions, 0))
                          | e -> e)
                 )
                 p
       | _ -> ()
    ) variables
in !ast


let compute_boundaries_in_c variables =
  Hashtbl.iter
    (fun name (level, p) ->
       if level != -1 then ()
       else begin
         List.iter (fun (permissions, iterators, accessors, _) ->
             let cm = build_corresponding_map iterators in
             let name_struct = name ^ "_infos" ^ "." ^ Variables.string_of_rw_flag permissions in
             List.iteri (fun i access ->
                 let _ = print_endline "{" in
                let _ = generate_reduction_in_c access cm "it_list" (name_struct ^ ".min") "min" in
                let _ = generate_reduction_in_c access cm "it_list" (name_struct ^ ".max") "max" in
                 print_endline "}" 
               )
               accessors
           ) 
           p
       end
    )
    variables


(* missing here: we must copy structs entirely! *)
let generate_transfer_in_openacc variables =
  let tbl = Hashtbl.create 0 in
  let add_directive flag name accessors =
    let parts = List.mapi (fun i _ ->
        let part = name ^ "_infos" ^"." ^ Variables.string_of_rw_flag flag in
        part ^ ".min", part ^ ".max"
      ) accessors
    in
    let spec = List.fold_left (fun a (b, b') -> a ^ "[" ^ b ^ ":" ^ b' ^ "]") "" parts in
    if Hashtbl.mem tbl name then
      ()
    else Hashtbl.add tbl name ((Variables.openacc_dir_of_flag flag)^"("^name^spec ^")")
  in 
  let _ = Hashtbl.iter
    (fun name (level, p) ->
       if level != -1 then ()
       else begin
         List.iter (fun (permissions, iterators, accessors, _) ->
             if permissions land Variables.is_function = Variables.is_function then
               ()
             else 
               add_directive permissions name accessors
           ) 
           p
       end
    )
    variables
in Hashtbl.iter (fun a b -> print_endline b) tbl
