open Ast
open Utils
open Prettyprint

let mk_ident i =
  Identifier (i, 0)
let mk_simple_type l =
  (l, (("", 0), [], DeBasic))
let mk_constant_int n =
  Constant(CInt(Dec, Num.num_of_int n, ""))
let mk_declaration simple_type name ast =
  Declaration (simple_type, [(name, -1), [], DeBasic, ast])



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
    let a, b = Hashtbl.fold (
      fun name l (expr_m, expr_M)  ->
        if l = [] then (expr_m, expr_M)
        else
          (* first, sum the parts of the computation *)
          let a = Calcul.convert_arithm_to_ast (Calcul.LAdd l)
          (* get the min and the max of this simple term *)
          in let mi, ma = 
               if name = "" then a, a
               else 
                 let l = BinaryOp(BinOp.Mul, a, fst @@ Hashtbl.find restricted name)
                 in let h = BinaryOp(BinOp.Mul, a, snd @@ Hashtbl.find restricted name) 
                 in let mi = Call(mk_ident "min", [l; h]) 
                 in let ma = Call(mk_ident "max", [l; h]) 
                 in mi, ma
          in match expr_m, expr_M with
          | Some expr_m, Some expr_M -> 
            (Some (BinaryOp(BinOp.Add, mi, expr_m)), Some (BinaryOp(BinOp.Add, ma, expr_M)))
          | x, y -> 
            (Some mi, Some ma)
    ) expression (None, None)
    in match (a, b) with
    | Some a, Some b -> a, b
    | _ -> failwith "shit happened"




(* create an hashmap linking iterators to a tuple
   (var_min, var_max), where var_min is the name of the variable containing the min
   and var_max the variable containing the max
*)
let create_it_hashmap ?(filter = fun x -> true) iterators =
  let its = Hashtbl.create (List.length iterators) 
  in let _ = List.iter 
         (fun (name', uuid', _) ->
            if filter uuid' then 
              let base = Access(Array, mk_ident "it_list", mk_constant_int uuid') 
              in Hashtbl.add its name' (Access(Member, base, mk_ident "min"), Access(Member, base, mk_ident "max"))
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





let rec code_from_tree out target tree itsname =
  match tree with
  | Calcul.TVal (op, expr, const) ->
    let mi, ma = expression_to_c expr itsname
    in Call(mk_ident "MAKE_OP_INTERVAL", 
         [String (Ast.BinOp.pretty_print op);
          mi; ma; 
          Calcul.convert_arithm_to_ast (Calcul.LAdd const);
          ]
        )
  | Calcul.TNone ->
    Call(mk_ident "MAKE_FULL_INTERVAL",
         [Access(Member, target, mk_ident "min");
         Access(Member, target, mk_ident "max");
         ])
  | Calcul.TAnd (a, b) ->
    let l = code_from_tree out target a itsname
    in let l' = code_from_tree out target b itsname
    in Call(mk_ident "INTER_INTERVAL",
            [l; l'])

  | Calcul.TOr (a, b) ->
    let l = code_from_tree out target a itsname
    in let l' = code_from_tree out target b itsname
    in Call(mk_ident "UNION_INTERVAL",
            [l; l'])


let max_uuid_iterators its =
  List.fold_left (fun old (_, uuid, _) -> 
      if old > uuid then old else uuid)
    (-1) its

(*
    Given all the variables, generate the code which will compute
   the bounds for the various iterators
*)
let create_iterators_in_c out variables =
  let iterators = get_iterators_from_variables variables 
  in let nb_iterators = max_uuid_iterators iterators + 1
  in let declaration_struct = 
    Declaration ([Struct("s_iterators", [])], 
                 [("it_list", -1), [], DeArray(DeBasic, [], DeArraySize(mk_constant_int nb_iterators)) , None ])
  in let content = List.fold_right (fun (name, uuid, constraints) old ->
             (* fold right to write in the good way iterators *)
      let its = create_it_hashmap iterators ~filter: (fun uuid' -> uuid' < uuid)
      in let target = Access(Array, mk_ident "it_list", mk_constant_int uuid)

     (* no need to reverse constraints now, it will be reversed during the process*)
      in let constraints = List.rev constraints
      in let content = List.fold_right 
        (fun c old ->  
          Assign(BinOp.Empty, target, 
                 Call(mk_ident "INTER_INTERVAL",
                      [target;
                       code_from_tree out target c its])) :: old
        )
        (List.tl constraints) 
        [Assign(BinOp.Empty, target, code_from_tree out target (List.hd constraints) its)]
      in List.rev content @ old
    ) iterators [] 
  in declaration_struct::content



let transform_code_par ast variables =
  let ast = ref ast 
  in let _ = Hashtbl.iter 
         (fun name p -> 
            List.iter
              (fun (permissions, _, _, uuids) ->
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


let generate_bounds_structures out array_summary clues =
  let code = Hashtbl.fold
    (fun name (_, size) old ->
       if size > 0 then begin
         let name_infos = Printf.sprintf "s_%s_infos" name
         in let decl_stmt = mk_declaration [Struct("s_infos", [])] name_infos None
         in let malloc_stmt =
              Cast(mk_simple_type [Int; Pointer],
                   Call(mk_ident "malloc", 
                        [BinaryOp(BinOp.Mul,
                                  UnaryOp(UnOp.SizeOf, Type( mk_simple_type [Int])),
                                  mk_constant_int size
                                 )]
                       ))
         in let bounds_stmts = List.fold_left 
                (fun old c ->
                   Assign(BinOp.Empty, Access(Member, Access(Member,
                                 mk_ident name_infos,
                                 mk_ident c),
                          mk_ident "min"), malloc_stmt)
                   :: Assign(BinOp.Empty, Access(Member, Access(Member,
                                 mk_ident name_infos,
                                 mk_ident c),
                          mk_ident "max"), malloc_stmt)
                 ::old
                ) [] ["f_w"; "f_r"; "f_rw"]
         in let ptr_stmts = List.fold_left
                (fun old c ->
                   (if Hashtbl.mem clues name then
                       let s, uuid, o, o' = Hashtbl.find clues name
                     in Declaration(s, [("s_" ^ name ^ "_" ^ c, -1), o, o', Some(mk_ident name)])
                   else 
                      let _ = Printf.printf "Type clue not found for variable %s, supposing it is a int*\n" name
                      in mk_declaration [Int; Pointer] ("s_" ^ name ^ "_" ^ c) (Some (mk_ident name))
                  ) :: old
                ) [] ["f_w"; "f_r"; "f_rw"]
         in let bloc = [decl_stmt] @ bounds_stmts @ ptr_stmts

    @ old

        in bloc

       end
       else 
         old
    ) array_summary [];
  in code





let transform_code_par ast variables =
  let ast = ref ast in
  let _ = Hashtbl.iter 
      (fun name p -> 
         List.iter
           (fun (permissions, _, _, uuids) ->
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
      | Identifier(name, u) ->
         Identifier("s_"^name^"_"^Variables.string_of_rw_flag permissions, u)
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
  let mk_array_member_access s fct i = 
    Access(Array, Access(Member, s, mk_ident fct), mk_constant_int i)
  in 
  let get_name2 name permission =
    Access(Member, mk_ident ("s_" ^ name ^ "_infos"), mk_ident (Variables.string_of_rw_flag permission))
  in
  let get_pragma name permission size =
    let name_infos = get_name2 name permission in
    let name = "s_" ^ name ^ "_" ^ Variables.string_of_rw_flag permission in
    let m = foldi(fun a i ->
        let fp = mk_array_member_access name_infos "min" i
        in let sp =
             BinaryOp(BinOp.Add,
             BinaryOp(BinOp.Sub, 
                      mk_array_member_access name_infos "max" i,
                      mk_array_member_access name_infos "min" i
                     ),
               mk_constant_int 1)
        in let content = pretty_print_ast fp ^ ":" ^ pretty_print_ast sp
        in Access(Array, a, mk_ident content)
      ) size (mk_ident name)
    in let w = Call(mk_ident @@ Variables.openacc_dir_of_flag permission, [m])
    in
    Preproc (Normal [
        "pragma";
        "acc";
        "data";
        pretty_print_ast w])

  in let keys = hashtbl_keys array_summary 
  in let rec aux ast l = 
       match l with
       | [] -> 
         Bloc([Preproc (Normal ["pragma"; "acc"; "kernels"])
               ;
               extract_bloc_content ast
              ])
       | name :: tl ->
         let uuids_hash, size = Hashtbl.find array_summary name 
         in let read_write_init = ref (Hashtbl.mem uuids_hash (Variables.read lor Variables.write))

         in let generate flag1 flag2 =
              if Hashtbl.mem uuids_hash flag1 && Hashtbl.mem uuids_hash flag2 then
                (
                  (Call(mk_ident "intersection_bounds",
                        [mk_constant_int size;
                         get_name2 name flag1;
                         get_name2 name flag2;
                        ]
                       ))
                ,
                ( let temp = (if !read_write_init then
                                 foldi (fun prev i ->
                                     let gname = get_name name in
                                     let temp s fct i = 
                                       Access(Array, Access(Member, mk_ident s, mk_ident fct), mk_constant_int i)
                                     in 
                                     Assign(BinOp.Empty, temp (gname Variables.readwrite) "min" i, 
                                            Call(mk_ident "min3",
                                                 [temp (gname Variables.readwrite) "min" i;
                                                  temp (gname flag1) "min" i;
                                                  temp (gname flag2) "min" i] )
                                           )
                                     ::
                                     Assign(BinOp.Empty, temp (gname Variables.readwrite) "max" i, 
                                            Call(mk_ident "max3",
                                                 [temp (gname Variables.readwrite) "max" i;
                                                  temp (gname flag1) "max" i;
                                                  temp (gname flag2) "max" i] )
                                           )
                                     :: prev
                                   ) size []
                               else 
                                 let _ = read_write_init := true in
                                 foldi (fun prev i ->
                                     let gname = get_name name in
                                     let temp s fct i = 
                                       Access(Array, Access(Member, mk_ident s, mk_ident fct), mk_constant_int i)
                                     in 
                                     Assign(BinOp.Empty, temp (gname Variables.readwrite) "min" i, 
                                            Call(mk_ident "min",
                                                 [temp (gname flag1) "min" i;
                                                  temp (gname flag2) "min" i] )
                                           )
                                     ::
                                     Assign(BinOp.Empty, temp (gname Variables.readwrite) "max" i, 
                                            Call(mk_ident "max",
                                                 [temp (gname flag1) "max" i;
                                                  temp (gname flag2) "max" i] )
                                           )
                                     :: prev
                                   ) size []
                              )
                   in temp
                 )
                 @
                 (
                   (if ((flag1 = Variables.readwrite && flag2 = Variables.read)
                        || (flag1 = Variables.read && flag2 = Variables.readwrite)) then
                      [get_pragma name (Variables.write) size ]
                    else 
                    if ((flag1 = Variables.readwrite && flag2 = Variables.write)
                        || (flag1 = Variables.write && flag2 = Variables.readwrite)) then
                      [get_pragma name (Variables.read) size ]
                    else []
                   )
                   @
                   [get_pragma name (Variables.readwrite) size ]
                 )
                 @
                 [(
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
                 )]
                )
                ::[]
              else []
         in let cond_parts = generate Variables.write Variables.read
                             @ generate Variables.write Variables.readwrite
                             @ generate Variables.read Variables.readwrite
         in let else_part = 
              ((if Hashtbl.mem uuids_hash Variables.read then 
                  [get_pragma name (Variables.read) size]
                else [])
               @
               (if Hashtbl.mem uuids_hash Variables.readwrite then 
                  [get_pragma name (Variables.readwrite) size]
                else [])
               @
               (if Hashtbl.mem uuids_hash Variables.write then 
                  [get_pragma name (Variables.write) size]
                else [])
              )
              @
              [(
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
              )]

         in let parts = 
              List.fold_left (fun old (cond, content) ->
                  IfThenElse(If, cond, (Bloc content), old)
                )
                (Bloc else_part) cond_parts
         in parts

  in aux ast keys :: []


let compute_boundaries_in_c out variables =
  let out_ast = ref []
  in let _ = Hashtbl.iter
    (fun name p ->
       List.iteri (fun i (permissions, iterators, accessors, _) ->
          let first_iteration = Hashtbl.create 3 
          in let its = create_it_hashmap iterators 
           in let its_list = List.map (fun (name, _, _) -> name) iterators 
           in let flag = Variables.string_of_rw_flag permissions 
           in let name_struct_ac = Access(Member, mk_ident @@ "s_" ^ name ^ "_infos", mk_ident flag)
           in List.iteri (fun i access ->
               let get_struct_member name =
                 Access(Array, Access(Member, name_struct_ac, mk_ident name), mk_constant_int i)
               in let small, huge = expression_to_c (Calcul.operate access its_list) its 
               in let statements = mk_declaration [Int] "__a" (Some small) 
                                   ::
                                   mk_declaration [Int] "__b" (Some huge) 
                                   :: []
               in let update = if not (Hashtbl.mem first_iteration (i, flag)) then
                      let _ = Hashtbl.add first_iteration (i, flag) true
                      in Assign(BinOp.Empty, 
                                get_struct_member "min",
                                Call(mk_ident "min", [mk_ident "__a"; mk_ident "__b"])
                               )
                         ::
                         Assign(BinOp.Empty, 
                                get_struct_member "max",
                                Call(mk_ident "max", [mk_ident "__a"; mk_ident "__b"])
                               )
                         :: []
                    else
                      Assign(BinOp.Empty, 
                             get_struct_member "min",
                             Call(mk_ident "min",
                                  [get_struct_member "min"; 
                                   Call(mk_ident "min", [mk_ident "__a"; mk_ident "__b"])]
                                 )
                            )
                      ::
                      Assign(BinOp.Empty, 
                             get_struct_member "max",
                             Call(mk_ident "max",
                                  [get_struct_member "max"; 
                                   Call(mk_ident "max", [mk_ident "__a"; mk_ident "__b"])]
                                 )
                            )
                      :: []

               in
               out_ast := Bloc (statements @ update)  :: !out_ast
             )
             accessors
         ) 
         p
    )
    variables
  in List.rev !out_ast



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
