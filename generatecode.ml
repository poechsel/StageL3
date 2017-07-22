open Ast
open Utils
open Prettyprint



(* compare to given iterators. 
   We compare them thanks to their uuid
*)
let iterators_compare (_, a, _) (_, b, _) =
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
         prev @ (List.fold_left (fun a (_, i, _, _) -> a @ i) [] p)
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
          let a = __print_list Calcul.pretty_print_arithm "+" l
          (* get the min and the max of this simple term *)
          in let mi, ma = 
               if name = "" then (a, a)
               else 
                 let a = if List.length l = 1 then a else "(" ^ a ^ ")" 
                 in let l = a ^ "*" ^ fst @@ Hashtbl.find restricted name 
                 in let h = a ^ "*" ^ snd @@ Hashtbl.find restricted name 
                 in let mi = "min(" ^ l ^ ", " ^ h ^ ")" 
                 in let ma = "max(" ^ l ^ ", " ^ h ^ ")" 
                 in mi, ma
          in (mi::expr_m, ma::expr_M)
    ) expression ([], [])
  (* finally, concat all parts *)
  in __print_list (fun x -> x) " + " l_min, 
     __print_list (fun x -> x) " + " l_max




(* create an hashmap linking iterators to a tuple
   (var_min, var_max), where var_min is the name of the variable containing the min
   and var_max the variable containing the max
*)
let create_it_hashmap ?(filter = fun x -> true) iterators =
  let its = Hashtbl.create (List.length iterators) 
  in let _ = List.iter 
         (fun (name', uuid', _) ->
            if filter uuid' then 
              let base = "it_list[" ^ string_of_int uuid' ^ "]" 
              in Hashtbl.add its name' (base ^ ".min", base ^ ".max")
            else
              ()
         ) iterators 
  in its


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
            let name = (name, (permissions land (Variables.write lor Variables.read))) 
            in if Hashtbl.mem out name then
              Hashtbl.replace out name (None, permissions, iterators, accessors, uuid)
            else 
              let reindex = if List.length accessors != 1 then
                  None
                else 
                  let its_list = List.map (fun (name, _, _) -> name) iterators 
                  in let expr = Calcul.operate (List.hd accessors) its_list
                  in if Calcul.is_expr_abi_form expr then
                    Some expr
                  else None
              in Hashtbl.add out name (reindex, permissions, iterators, accessors, uuid)
          ) vars
    ) variables
  in out

let iterator_constraint_in_c out target (uuid, op, ineq, div) its_names =
  let ineq_min, ineq_max = expression_to_c ineq its_names
  in let ineq_min = 
       Printf.sprintf "(%s)/-(%s)" ineq_min (__print_list Calcul.pretty_print_arithm "+" div)
  in let ineq_max = 
       Printf.sprintf "(%s)/-(%s)" ineq_max (__print_list Calcul.pretty_print_arithm "+" div)
  in let _ = 
       Printf.fprintf out "// %s %s\n" (BinOp.pretty_print op) ineq_min ;

  in match uuid with 
  | ItStart -> 
    Printf.fprintf out "%s.min = %s;\n" target @@ ineq_min
  | ItStop ->
    Printf.fprintf out "%s.max = %s;\n" target @@ ineq_max
  | _ -> begin
      match op with
      | BinOp.Eq | BinOp.Neq ->
        Printf.fprintf out "%s.min = min(%s.min, %s);\n" target target ineq_min;
        Printf.fprintf out "%s.max = max(%s.max, %s);\n" target target ineq_max
      | BinOp.Slt ->
        Printf.fprintf out "%s.max = min(%s.max, %s);\n" target target ineq_max
      | BinOp.Sgt ->
        Printf.fprintf out "%s.min = max(%s.min, %s);\n" target target ineq_max
      | BinOp.Leq ->
        (* not sure for the +1 *)
        Printf.fprintf out "%s.max = min(%s.max, %s+1);\n" target target ineq_max
      | BinOp.Geq ->
        Printf.fprintf out "%s.min = max(%s.min, %s+1);\n" target target ineq_max
      | _ -> ()
    end




let rec code_from_tree out target tree itsname =
  match tree with
  | Calcul.TVal (op, expr, const) ->
    let mi, ma = expression_to_c expr itsname
    in Printf.sprintf "MAKE_OP_INTERVAL(\"%s\", %s, %s, %s, \"%s\")"
      (Ast.BinOp.pretty_print op)
      mi
      ma
      (__print_list Calcul.pretty_print_arithm "+" const)
      target
  | Calcul.TNone ->
    Printf.sprintf  "MAKE_FULL_INTERVAL(%s.min, %s.max)"  target target
  | Calcul.TAnd (a, b) ->
    let l = code_from_tree out target a itsname
    in let l' = code_from_tree out target b itsname
    in Printf.sprintf "INTER_INTERVAL(\n%s,\n%s)" l l'

  | Calcul.TOr (a, b) ->
    let l = code_from_tree out target a itsname
    in let l' = code_from_tree out target b itsname
    in Printf.sprintf "UNION_INTERVAL(\n%s,\n%s)" l l'

(*
    Given all the variables, generate the code which will compute
   the bounds for the various iterators
*)
let create_iterators_in_c out variables =
  let iterators = get_iterators_from_variables variables 
  in let _ = Printf.fprintf out "s_iterators it_list[%d];\n" (List.length iterators) 
  in let _ = List.iter (fun (name, uuid, constraints) ->
      let its = create_it_hashmap iterators ~filter: (fun uuid' -> uuid' < uuid)
      (*let start = Calcul.operate start its_list in
        let stop = Calcul.operate stop its_list in
      *)in let target = "it_list[" ^ string_of_int uuid ^ "]" 
      in 
      (*let _ = Printf.fprintf out "%s.min = %s;\n" target @@ fst @@ expression_to_c start its in
        Printf.fprintf out "%s.max = %s;\n" target @@ snd @@ expression_to_c stop its 
      *)


      (*let constraints = List.sort (fun (a, _, _ ,_) (b, _, _, _) -> Pervasives.compare a b) constraints in*)
      List.iter (fun c ->  Printf.fprintf out "%s = %s;\n" target @@ code_from_tree out target c its) (List.rev constraints)


    ) iterators
  in ()



let transform_code_par ast variables =
  let ast = ref ast 
  in let _ = Hashtbl.iter 
         (fun name p -> 
            List.iter
              (fun (permissions, _, _, uuids) ->
                 (*       let _ = print_endline @@ "seeing " ^ name  ^ " id = " ^ (__print_list string_of_int "," uuids) in*)
                 if Variables.is_array_flag permissions then
                   ast := Variables.rename !ast uuids (function
                       | Identifier(name, u) ->Access(Member, Identifier("s_"^name, u), Identifier(Variables.string_of_rw_flag permissions, 0))
                       | e -> e)
              )
              p
         ) variables
  in !ast



(* return a hashtbl of the form:
   name <=> ( uuids hash (this is an hasmap permission -> uuids)), size )
*)
let get_array_summary variables = 
  let out = Hashtbl.create 0 
  in let _ = Hashtbl.iter
         (fun name p ->
            let uuid_hash = Hashtbl.create 0 
            in let size, a = List.fold_left (fun (previous_size, a) (permissions, _, accessors, uuids) ->
                let a = a || (Variables.is_array_flag permissions)
                in let permissions = permissions land (Variables.write lor Variables.read) 
                in let size = 
                     if List.length accessors > previous_size then
                       List.length accessors 
                     else previous_size
                in let _ = if Hashtbl.mem uuid_hash permissions then
                       Hashtbl.replace uuid_hash permissions (Hashtbl.find uuid_hash permissions @ uuids)
                     else
                       Hashtbl.add uuid_hash permissions uuids
                in (size, a))
                (0, false) p
            in if a then  Hashtbl.add out name (uuid_hash, size)
         ) variables
  in out



let generate_bounds_structures out array_summary =
  Printf.fprintf out "\nGenerating structures: \n";
  Hashtbl.iter
    (fun name (_, size) ->
       if size > 0 then begin
         let name_infos = Printf.sprintf "s_%s_infos" name 
         in let _ = Printf.fprintf out "s_infos %s;\n" name_infos 
         in let e = Printf.sprintf 
                "(int*)malloc(sizeof(int) * %d);" size 
         in let _ = List.iter
                (fun p -> Printf.fprintf out
                    "%s.%s.min = %s\n%s.%s.max = %s\n"
                    name_infos p e name_infos p e
                ) ["f_w"; "f_r"; "f_rw"]
         in let _ = Printf.fprintf out "s_array s_%s;\n" name
         in let _ = List.iter
                (fun p -> Printf.fprintf out
                    "s_%s.%s = %s;\n"
                    name p name
                ) ["f_w"; "f_r"; "f_rw"]
         in ()
       end
    ) array_summary;
  Printf.fprintf out "\n"





let transform_code_par ast variables =
  let ast = ref ast in
  let _ = Hashtbl.iter 
      (fun name p -> 
         List.iter
           (fun (permissions, _, _, uuids) ->
              (*       let _ = print_endline @@ "seeing " ^ name  ^ " id = " ^ (__print_list string_of_int "," uuids) in*)
              if Variables.is_array_flag permissions then
                ast := Variables.rename !ast uuids (function
                    | Identifier(name, u) ->Access(Member, Identifier("s_"^name, u), Identifier(Variables.string_of_rw_flag permissions, 0))
                    | e -> e)
           )
           p
      ) variables
  in !ast

let transform_code_identifiers permissions uuids ast =
  Variables.rename ast uuids (function
      | Identifier(name, u) ->Access(Member, Identifier("s_"^name, u), Identifier(Variables.string_of_rw_flag permissions, 0))
      | e -> e)



let rec foldi fct i init =
  if i <= 0 then
    init
  else 
    fct (foldi fct (i-1) init) (i-1)


let rec extract_bloc_content b =
  match b with
  | Bloc([x]) -> extract_bloc_content x
  | x -> x

let generate_parallel_loop out ast array_summary =
  let get_name name permission =
    "s_" ^ name ^ "_infos." ^ Variables.string_of_rw_flag permission
  in
  let get_pragma name permission size =
    let name_infos = get_name name permission in
    let name = "s_" ^ name ^ "." ^ Variables.string_of_rw_flag permission in
    let m = foldi(fun a i ->
        a ^ "[" ^ (name_infos ^ ".min" ^ "[" ^ string_of_int i ^ "]") ^ ":" ^
        (name_infos ^ ".max" ^ "[" ^ string_of_int i ^ "]") ^
        " - " ^
        (name_infos ^ ".min" ^ "[" ^ string_of_int i ^ "]") ^
        "]"
      ) size ""
    in
    "#pragma acc data " ^ Variables.openacc_dir_of_flag permission ^ "(" ^
    name ^ m ^ ")"

  in let keys = hashtbl_keys array_summary 
  in let rec aux ast l = 
       match l with
       | [] -> "#pragma acc data kernels\n" ^
               Printf.sprintf "%s\n" (pretty_print_ast @@ extract_bloc_content ast)
       | name :: tl ->
         let uuids_hash, size = Hashtbl.find array_summary name 
         in let read_write_init = ref (Hashtbl.mem uuids_hash (Variables.read lor Variables.write))
         in let pragma_structure = "#pragma acc copy(s_" ^ name ^ ")\n"

         in let generate flag1 flag2 =
              if Hashtbl.mem uuids_hash flag1 && Hashtbl.mem uuids_hash flag2 then
                (Printf.sprintf "intersection_bounds(%d, %s, %s)" size (get_name name flag1) (get_name name flag2),
                 (if !read_write_init then
                    foldi (fun prev i ->
                        let gname = get_name name in
                        prev 
                        ^
                        Printf.sprintf "%s.min[%d] = min3(%s.min[%d], %s.min[%d], %s.min[%d]);\n"
                          (gname Variables.readwrite) i
                          (gname Variables.readwrite) i
                          (gname flag1) i
                          (gname flag2) i
                        ^
                        Printf.sprintf "%s.max[%d] = max3(%s.max[%d], %s.max[%d], %s.max[%d]);\n"
                          (gname Variables.readwrite) i
                          (gname Variables.readwrite) i
                          (gname flag1) i
                          (gname flag2) i

                      ) size ""
                  else 
                    let _ = read_write_init := true in
                    foldi (fun prev i ->
                        let gname = get_name name in
                        prev 
                        ^
                        Printf.sprintf "%s.min[%d] = min(%s.min[%d], %s.min[%d]);\n"
                          (gname Variables.readwrite) i
                          (gname flag1) i
                          (gname flag2) i
                        ^
                        Printf.sprintf "%s.max[%d] = max(%s.max[%d], %s.max[%d]);\n"
                          (gname Variables.readwrite) i
                          (gname flag1) i
                          (gname flag2) i

                      ) size ""
                 )
                 ,
                 pragma_structure ^
                 (if ((flag1 = Variables.readwrite && flag2 = Variables.read)
                      || (flag1 = Variables.read && flag2 = Variables.readwrite)) then
                    get_pragma name (Variables.write) size ^ "\n"
                  else 
                  if ((flag1 = Variables.readwrite && flag2 = Variables.write)
                      || (flag1 = Variables.write && flag2 = Variables.readwrite)) then
                    get_pragma name (Variables.read) size ^ "\n"
                  else ""
                 )
                 ^
                 get_pragma name (Variables.readwrite) size ^ "\n" 
                 ,
                 aux (transform_code_identifiers (flag2 lor flag1) 
                        ((Hashtbl.find uuids_hash flag1)
                         @ (Hashtbl.find uuids_hash flag2))

                        (if Hashtbl.mem uuids_hash (flag2 lor flag1) then
                           transform_code_identifiers (flag2 lor flag1)
                             (Hashtbl.find uuids_hash (flag2 lor flag1))
                             ast
                         else ast
                        )
                     ) tl
                )::[]
              else []
         in let cond_parts = generate Variables.write Variables.read
                             @ generate Variables.write Variables.readwrite
                             @ generate Variables.read Variables.readwrite
         in let else_part = 
              ("", 
               "",
               pragma_structure ^
               ((if Hashtbl.mem uuids_hash Variables.read then 
                   get_pragma name (Variables.read) size ^ "\n"
                 else "")
                ^
                (if Hashtbl.mem uuids_hash Variables.readwrite then 
                   get_pragma name (Variables.readwrite) size ^ "\n"
                 else "")
                ^
                (if Hashtbl.mem uuids_hash Variables.write then 
                   get_pragma name (Variables.write) size ^ "\n"
                 else "")
               ),
               let ast  = if Hashtbl.mem uuids_hash Variables.read then 
                   transform_code_identifiers (Variables.read) (Hashtbl.find uuids_hash Variables.read) ast
                 else ast
               in let ast = if Hashtbl.mem uuids_hash Variables.readwrite then 
                      transform_code_identifiers (Variables.readwrite) (Hashtbl.find uuids_hash Variables.readwrite) ast
                    else ast
               in let ast = if Hashtbl.mem uuids_hash Variables.write then 
                      transform_code_identifiers (Variables.write) (Hashtbl.find uuids_hash Variables.write) ast
                    else ast
               in aux ast tl
              )

         in let parts = List.map (fun (cond, update, preproc, content) ->
             Printf.sprintf "if (%s) {\n%s\n%s\n%s}\n" cond update preproc content) cond_parts
         in let parts = 
              let cond, update, preproc, content = else_part in
              let s = Printf.sprintf "{\n%s\n%s\n%s}\n" update preproc content
              in parts @ [s]
         in __print_list (fun x -> x) "else" parts

  in Printf.fprintf out "%s\n" @@ aux ast keys


let compute_boundaries_in_c out variables =
  Hashtbl.iter
    (fun name p ->
       let first_iteration = Hashtbl.create 3 in
       List.iteri (fun i (permissions, iterators, accessors, _) ->
           let its = create_it_hashmap iterators in
           let its_list = List.map (fun (name, _, _) -> name) iterators in
           let flag = Variables.string_of_rw_flag permissions in 
           let name_struct = "s_" ^ name ^ "_infos" ^ "." ^ flag in
           List.iteri (fun i access ->
               let _ = Printf.fprintf out "{\n" in
               let small, huge = expression_to_c (Calcul.operate access its_list) its in
               let _ = Printf.fprintf out "int ___a = %s;\n" @@ small in
               let _ = Printf.fprintf out "int ___b = %s;\n" @@ huge in
               let _ = if not (Hashtbl.mem first_iteration flag) then
                   let _ = Printf.fprintf out "%s.min[%d] = min(___a, ___b);\n" name_struct i in
                   let _ = Printf.fprintf out "%s.max[%d] = max(___a, ___b);\n" name_struct i in
                   Hashtbl.add first_iteration flag true
                 else
                   let _ = Printf.fprintf out "%s.min[%d] = min(%s.min[%d], min(___a, ___b));\n" name_struct i name_struct i in
                   Printf.fprintf out "%s.max[%d] = max(%s.max[%d], max(___a, ___b));\n" name_struct i name_struct i in
               Printf.fprintf out "}\n" 
             )
             accessors
         ) 
         p
    )
    variables



(* missing here: we must copy structs entirely! *)
let generate_transfer_in_openacc out variables =
  let tbl = Hashtbl.create 0 
  in let add_directive flag name accessors =
       let parts = List.mapi (fun i _ ->
           let part = name ^ "_infos" ^"." ^ Variables.string_of_rw_flag flag 
           in part ^ ".min[" ^ string_of_int i ^ "]", part ^ ".max[" ^ string_of_int i ^ "]"
         ) accessors
       in
       let spec = List.fold_left (fun a (b, b') -> a ^ "[" ^ b ^ ":" ^ b' ^ "]") "" parts 
       in if Hashtbl.mem tbl name then
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
  in Hashtbl.iter (fun a b -> Printf.fprintf out "%s\n" b) tbl



(* a constrait is a tuple
   (op, iterator name, right side, denom)
   It represents iterantor_name * denom OP right side
*) 
