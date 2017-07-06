open Ast
open Prettyprint


let iterators_compare (_, a, _, _, _) (_, b, _, _, _) =
  if a < b then -1
  else if a = b then 0
  else 1


(* return a list of iterators from the variables 

   Summary:
   iterate over every variables and concat the iterators
   Then, with sort_uniq it will erase distinct iterators

   Iterators are identified by their uuids, not their name!
*)
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








(*
Return the keys of an hashtbl
*)
let hashtbl_keys tbl = 
  let l = Hashtbl.fold (fun key _ p -> key :: p) tbl []
  in List.sort_uniq Pervasives.compare l


(*
   Given an expression, return two strings representing an expression
   giving an upper bound and an lower bound of it.
   It work because the expressions are linear polynoms.

   Expression is given in the form outputted by the formal calculus, as an:
   Hashmap of unknown (an iterator name) -> list of products
   Therefore, an expression is given as:
   sum (iterator_value * (sum of content of the hashtbl))

   restricted is an hashmap affecting to an iterator name a tuple
   (str_min, str_max) where str_min is a string referencing to the min value of 
   the iterator and str_max to the max value
*)
let expression_to_c expression restricted =
  let l_min, l_max = Hashtbl.fold (
      fun name l (expr_m, expr_M)  ->
        if l = [] then (expr_m, expr_M)
        else
          (* first, sum the parts of the computation *)
          let a = __print_list Calcul.pretty_print_arithm "+" l in
          (* get the min and the max of this simple term *)
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
  (* finally, concat all parts *)
  in __print_list (fun x -> x) " + " l_min, 
     __print_list (fun x -> x) " + " l_max


let create_iterators_in_c variables =
  let iterators = get_iterators_from_variables variables in
  let its = Hashtbl.create (List.length iterators) in
  let _ = List.iter 
      (fun (name, uuid, _, _, _) ->
         let base = "it_list[" ^ string_of_int uuid ^ "]" in
         Hashtbl.add its name (base ^ ".min", base ^ ".max")
      ) iterators in
  let its_list =  hashtbl_keys its in
  let _ = Printf.printf "s_iterators it_list[%d];\n" (List.length iterators) in
  let _ = List.iter (fun ((name, uuid, start, stop, _)) ->
      let start = Calcul.operate start its_list in
      let stop = Calcul.operate stop its_list in
      let target = "it_list[" ^ string_of_int uuid ^ "]" in
      let _ = Printf.printf "%s.min = %s;\n" target @@ fst @@ expression_to_c start its in
      Printf.printf "%s.max = %s;\n" target @@ snd @@ expression_to_c stop its 
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
       let first_iteration = Hashtbl.create 3 in
       if level != -1 then ()
       else begin
         List.iter (fun (permissions, iterators, accessors, _) ->
             let its = Hashtbl.create (List.length iterators) in
             let _ = List.iter 
                 (fun (name, uuid, _, _, _) ->
                    let base = "it_list[" ^ string_of_int uuid ^ "]" in
                    Hashtbl.add its name (base ^ ".min", base ^ ".max")
                 ) iterators in
             let its_list = List.map (fun (name, _, _, _, _) -> name) iterators in
             let flag = Variables.string_of_rw_flag permissions in 
             let name_struct = name ^ "_infos" ^ "." ^ flag in
             List.iteri (fun i access ->
                 let _ = print_endline "{" in
                 let small, huge = expression_to_c (Calcul.operate access its_list) its in
                 let _ = Printf.printf "int ___a = %s;\n" @@ small in
                 let _ = Printf.printf "int ___b = %s;\n" @@ huge in
                 let _ = if not (Hashtbl.mem first_iteration flag) then
                 let _ = Printf.printf "%s.min = min(___a, ___b);\n" name_struct in
                 let _ = Printf.printf "%s.max = max(___a, ___b);\n" name_struct in
                 Hashtbl.add first_iteration flag true
                 else
                 let _ = Printf.printf "%s.min = min(%s.min, min(___a, ___b));\n" name_struct name_struct in
                  Printf.printf "%s.max = max(%s.max, max(___a, ___b));\n" name_struct name_struct in
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
    let _ = print_endline spec in
    let _ = print_endline name in
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
               let name = if permissions land Variables.is_array = Variables.is_array
                 then 
                   "s_" ^ name ^ "." ^ Variables.string_of_rw_flag permissions 
else name in
               let _ = Printf.printf "%s : %s\n" (name) (Variables.string_of_rw_flag permissions) in
               add_directive permissions name accessors
           ) 
           p
       end
    )
    variables
in Hashtbl.iter (fun a b -> print_endline b) tbl
