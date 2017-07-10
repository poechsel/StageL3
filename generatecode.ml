open Ast
open Prettyprint



(* compare to given iterators. 
   We compare them thanks to their uuid
*)
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
    (fun name p prev ->
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
 (* let _ = Hashtbl.iter
            (fun name content ->
          let _ = print_endline name 
          in List.iter (fun x -> print_endline @@ "  " ^ Calcul.pretty_print_arithm x) content
        ) expression in*)
  let l_min, l_max = Hashtbl.fold (
      fun name l (expr_m, expr_M)  ->
             (* let _ = print_endline @@ "-> " ^ name in*)
        if l = [] then (expr_m, expr_M)
        else
          (* first, sum the parts of the computation *)
          let a = __print_list Calcul.pretty_print_arithm "+" l in
          (* get the min and the max of this simple term *)
          let mi, ma = 
            if name = "" then (a, a)
            else 
              let a = if List.length l = 1 then a else "(" ^ a ^ ")" in
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




(* create an hashmap linking iterators to a tuple
   (var_min, var_max), where var_min is the name of the variable containing the min
   and var_max the variable containing the max
*)
let create_it_hashmap ?(filter = fun x -> true) iterators =
      let its = Hashtbl.create (List.length iterators) in
      let _ = List.iter 
          (fun (name', uuid', _, _, _) ->
             if filter uuid' then 
             let base = "it_list[" ^ string_of_int uuid' ^ "]" in
             Hashtbl.add its name' (base ^ ".min", base ^ ".max")
               else
                 ()
          ) iterators in
      its


(* give back an hashmap associating to each variable if we can reindex it or not
   A variable can be reindex if it is:
    - a single dimension array
    - it's access expression is "unique" <- could be change in the future, don't think so
*)
let get_reindexable_vars variables =
  let out = Hashtbl.create 0 
  in let _ = Hashtbl.iter (
      fun name vars ->
        List.iter (fun (permissions, iterators, accessors, uuid) ->
            (* here is a small hack. We use the key (name, permission) to distinguish
               the accesses*)
            let name = (name, (permissions land (Variables.write lor Variables.read))) in
            if Hashtbl.mem out name then
              Hashtbl.replace out name (None, permissions, iterators, accessors, uuid)
            else 
              let reindex = if List.length accessors != 1 then
                  None
                else 
                  let its_list = List.map (fun (name, _, _, _, _) -> name) iterators 
                  in let expr = Calcul.operate (List.hd accessors) its_list
                  in if Calcul.is_expr_abi_form expr then
                    Some expr
                  else None
              in Hashtbl.add out name (reindex, permissions, iterators, accessors, uuid)
          ) vars
    ) variables
  in out


(*
    Given all the variables, generate the code which will compute
   the bounds for the various iterators
*)
let create_iterators_in_c variables =
  let iterators = get_iterators_from_variables variables in

  let _ = Printf.printf "s_iterators it_list[%d];\n" (List.length iterators) in
  let _ = List.iter (fun (name, uuid, start, stop, _) ->
      let its = create_it_hashmap iterators ~filter: (fun uuid' -> uuid' < uuid)
          in
      let its_list =  hashtbl_keys its in
      let start = Calcul.operate start its_list in
      let stop = Calcul.operate stop its_list in
      let target = "it_list[" ^ string_of_int uuid ^ "]" in
      let _ = Printf.printf "%s.min = %s;\n" target @@ fst @@ expression_to_c start its in
      Printf.printf "%s.max = %s;\n" target @@ snd @@ expression_to_c stop its 
    ) iterators
  in ()



let transform_code_par ast variables =
  let ast = ref ast in
  let _ = Hashtbl.iter 
    (fun name p -> 
      List.iter
                 (fun (permissions, _, _, uuids) ->
                    if permissions land Variables.is_function = Variables.is_function then
                      ()
                    else 
               (*       let _ = print_endline @@ "seeing " ^ name  ^ " id = " ^ (__print_list string_of_int "," uuids) in*)
                      if permissions land Variables.is_array = Variables.is_array then
                      ast := Variables.rename !ast uuids (function
                          | Identifier(name, u) ->Access(Member, Identifier("s_"^name, u), Identifier(Variables.string_of_rw_flag permissions, 0))
                          | e -> e)
                 )
                 p
    ) variables
in !ast




let generate_bounds_structures variables =
  print_endline "\nGenerating structures: \n";
  Hashtbl.iter
    (fun name p ->
       let size = 
         List.fold_left 
           (fun s (_, _, accessors, _) ->
              let l = List.length accessors in
              if l > s then l else s 
           ) 0 p
       in if size > 0 then begin
         let e = Printf.sprintf 
             "(int*)malloc(sizeof(int) * %d);" size in
         List.iter
           (fun p -> Printf.printf
               "%s.%s.min %s\n%s.%s.max %s\n"
               name p e name p e
           ) ["f_w"; "f_r"; "f_rw"]
       end
    ) variables;
  print_endline "\n"


let compute_boundaries_in_c variables =
  Hashtbl.iter
    (fun name p ->
       let first_iteration = Hashtbl.create 3 in
         List.iteri (fun i (permissions, iterators, accessors, _) ->
             let its = create_it_hashmap iterators in
             let its_list = List.map (fun (name, _, _, _, _) -> name) iterators in
             let flag = Variables.string_of_rw_flag permissions in 
             let name_struct = name ^ "_infos" ^ "." ^ flag in
             List.iteri (fun i access ->
                 let _ = print_endline "{" in
                 let small, huge = expression_to_c (Calcul.operate access its_list) its in
                 let _ = Printf.printf "int ___a = %s;\n" @@ small in
                 let _ = Printf.printf "int ___b = %s;\n" @@ huge in
                 let _ = if not (Hashtbl.mem first_iteration flag) then
                     let _ = Printf.printf "%s.min[%d] = min(___a, ___b);\n" name_struct i in
                     let _ = Printf.printf "%s.max[%d] = max(___a, ___b);\n" name_struct i in
                     Hashtbl.add first_iteration flag true
                   else
                     let _ = Printf.printf "%s.min[%d] = min(%s.min[%d], min(___a, ___b));\n" name_struct i name_struct i in
                     Printf.printf "%s.max[%d] = max(%s.max[%d], max(___a, ___b));\n" name_struct i name_struct i in
                 print_endline "}" 
               )
               accessors
           ) 
           p
    )
    variables



(* missing here: we must copy structs entirely! *)
let generate_transfer_in_openacc variables =
  let tbl = Hashtbl.create 0 in
  let add_directive flag name accessors =
    let parts = List.mapi (fun i _ ->
        let part = name ^ "_infos" ^"." ^ Variables.string_of_rw_flag flag in
        part ^ ".min[" ^ string_of_int i ^ "]", part ^ ".max[" ^ string_of_int i ^ "]"
      ) accessors
    in
    let spec = List.fold_left (fun a (b, b') -> a ^ "[" ^ b ^ ":" ^ b' ^ "]") "" parts in
    if Hashtbl.mem tbl name then
      ()
    else Hashtbl.add tbl name ((Variables.openacc_dir_of_flag flag)^"("^name^spec ^")")
  in 
  let _ = Hashtbl.iter
    (fun name p ->
         List.iteri (fun i (permissions, iterators, accessors, _) ->
             if permissions land Variables.is_function = Variables.is_function then
               ()
             else 
               let name = if permissions land Variables.is_array = Variables.is_array
                 then 
                   "s_" ^ name ^ "." ^ Variables.string_of_rw_flag permissions 
else name in
               add_directive permissions name accessors
           ) 
           p
    )
    variables
in Hashtbl.iter (fun a b -> print_endline b) tbl
