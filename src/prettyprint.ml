open Ast


let indentation_level = ref 0
let mk_indent () = String.make !indentation_level ' '
let __print_list fct sep l =
  String.concat sep (List.map fct l);;

let rec pretty_print_ast ast = 
  match ast with
  | Identifier (s, u) -> s (*"["^s^"="^string_of_int u^"]#"*)

  | InitializerList l -> 
    "{" ^ 
    if l = [] then "" 
    else __print_list pretty_print_ast ", " l
         ^ "}"

  | Constant c ->
    pretty_print_constant c

  | String c ->
    "\"" ^ c ^ "\""

  | Call(fct, args) ->
    pretty_print_ast fct ^
    "(" ^
    __print_list pretty_print_ast ", " args ^
    ")"

  | Access(met, from, which) ->
    Printf.sprintf (pretty_print_access_method met) (pretty_print_ast from) (pretty_print_ast which)

  | UnaryOp(op, ast) ->
    UnOp.pretty_print (pretty_print_ast ast) op

  | BinaryOp(op, a, b) ->
    begin match a with
    | BinaryOp(op', _, _) when BinOp.get_weight op' > BinOp.get_weight op ->
      "(" ^ pretty_print_ast a ^ ")"
    | _ -> pretty_print_ast a
    end 
    ^ BinOp.pretty_print op ^
    begin match b with
    | BinaryOp(op', _, _) when BinOp.get_weight op' > BinOp.get_weight op ->
      "(" ^ pretty_print_ast b ^ ")"
    | _ -> pretty_print_ast b
    end 

  | Assign(op, a, b) ->
    pretty_print_ast a ^ " " ^ BinOp.pretty_print op ^ "= " ^ pretty_print_ast b

  | Expression l ->
    __print_list (pretty_print_ast) ", " l

  | IfThenElse(t, cond, if_sts, else_sts) ->
    let s_if = pretty_print_line if_sts 
    in let s_else = pretty_print_line else_sts 
    in begin match t with
      | Ternary -> "((" ^ pretty_print_ast cond ^ ") ? " ^ s_if ^ " : " ^ s_else ^ ")" 
      | _ -> 
        Printf.sprintf "if (%s)%s" (pretty_print_ast cond) s_if
        ^ match else_sts with
        | Bloc [] -> ""
        | x -> mk_indent () ^ "else " ^ s_else
    end

  | Return None -> 
    "return"

  | Return (Some e) -> 
    "return " ^ pretty_print_ast e 

  | Break -> 
    "break"

  | Continue -> 
    "continue"

  | Goto s -> 
    "goto " ^ s

  | Default s -> 
    "default :" ^ pretty_print_ast s

  | Label (n, s) -> 
    n ^ " :" ^ pretty_print_ast s

  | Case (a, b) -> 
    "case " ^ pretty_print_ast a ^ ":" ^ pretty_print_ast b

  | Bloc l -> 
    let begin_bracket = "\n" ^ mk_indent () ^ "{\n" 
    in let _ = incr indentation_level 
    in let content =  
         __print_list (fun a -> mk_indent () ^ pretty_print_ast a ^ 
                                match a with
                                | Bloc _ | Preproc _ | For _ | While (NoWhile, _, _) | IfThenElse _ -> ""
                                | _ -> ";"
                      ) "\n" l 
    in let _ = decr indentation_level 
    in let end_bracket = "\n" ^ mk_indent () ^ "}\n" 
    in begin_bracket ^ content ^ end_bracket

  | Switch (expr, l) -> 
    "switch (" ^ pretty_print_ast expr ^ ")" ^ pretty_print_ast l 

  | Type t ->
    pretty_print_type t

  | Cast (t, e) -> 
    Printf.sprintf "(%s) %s" (pretty_print_type t) (pretty_print_ast e)

  | For (a, b, c, content) -> 
    let aux x = match x with 
      | None -> "" 
      | Some x -> pretty_print_ast x
    in Printf.sprintf "for (%s; %s; %s)%s" (aux a) (aux b) (aux c) (pretty_print_ast content)

  | While (t, cond, content) ->
    Printf.sprintf (match t with 
        | DoWhile -> "do%swhile(%s)"
        | NoWhile -> "while(%s) %s")
      (pretty_print_ast cond)
      (pretty_print_ast content)

  | Declaration (spec, l) ->
    pretty_print_decl_spec spec ^ " " ^
    __print_list (fun ((name, uuid) , spec, decl, ast) ->
        pretty_print_decl_spec spec ^ " " ^ pretty_print_decl decl name ^ 
        (match ast with 
         | None -> "" 
         | Some a -> " = " ^ pretty_print_ast a)
      ) ", " l

  | FunctionDeclaration(spec, decl, other, content) ->
    pretty_print_decl_spec spec ^ " " ^
    pretty_print_decl_type decl ^ " " ^
    __print_list pretty_print_ast " " other ^ "\n" ^
    pretty_print_ast content 

  | Preproc (Normal l) ->
    "#" ^ __print_list (fun x -> x) " " l 
  | Preproc (Custom decl) ->
    ""

and pretty_print_line l =
  match l with
  | IfThenElse _ | While _ | For _ | Declaration _ | Bloc _ -> pretty_print_ast l
  | x -> pretty_print_ast x ^ ";\n"

and pretty_print_type (spec, t) = 
  let dt = pretty_print_decl_type t
  in let dt = String.trim dt
  in pretty_print_decl_spec spec ^ (if String.length dt > 0 then " " ^ dt else "")

and pretty_print_access_method m = match m with
  | Array -> "%s[%s]"
  | Member -> "%s.%s"
  | Pointer -> "%s->%s"

and pretty_print_constant const = 
  match const with
  | CChar c -> c
  | CInt (base, num, flag) -> Num.string_of_num num ^ flag
  | CFloat (base, num, decimal, Some exposant, flag) -> Num.string_of_num num ^ "." ^ Num.string_of_num decimal ^ pretty_print_exposant exposant ^ flag
  | CFloat (base, num, decimal, None, flag) -> Num.string_of_num num ^ "." ^ Num.string_of_num decimal ^ flag

and pretty_print_exposant (Exponent (s, sign, e)) = 
  String.make 1 s ^ (if sign > 0 then "+" else "-") ^ Num.string_of_num e

and pretty_print_declaration_specifiers = function  
  | Typedef -> "typedef"
  | Extern  -> "extern"
  | Static -> "static"
  | Auto    -> "auto"
  | Register -> "register"
  | Void    -> "void"
  | Char    -> "char"
  | Short   -> "short"
  | Int     -> "int"
  | Long    -> "long"
  | Float   -> "float"
  | Double  -> "double"
  | Signed  -> "signed"
  | Unsigned    -> "unsigned"
  | Bool    -> "__Bool"
  | Complex -> "__Complex"
  | Struct (a, b) -> "struct " ^ pretty_print_struct (a, b)
  | Union (a, b) -> "union " ^ pretty_print_struct (a, b)
  | Enum (a, b) -> "enum " ^ pretty_print_enum (a, b)
  | Const -> "const"
  | Volatile    -> "volatile"
  | Restrict -> "restrict"
  | Inline  -> "inline"
  | Pointer -> "*"

and pretty_print_struct s =
  let rec aux l = match l with
    | [] -> ""
    | (spec, name, respec, decl, bit_size)::tl ->
      pretty_print_decl_spec spec ^ " " ^
      pretty_print_decl_spec respec ^ " " ^
      pretty_print_decl decl name ^ " " ^
      (match bit_size with | None -> "" | Some bit_size -> " : " ^ pretty_print_ast bit_size) ^ "\n" ^
      aux tl
  in
  let name, entries = s 
  in match entries with
  | [] -> name
  | l   -> name ^ "{\n" ^ aux l ^ "}"

and pretty_print_enum s =
  let rec aux l = match l with
    | [] -> ""
    | (name, None)::tl -> name ^ ",\n" ^ aux tl
    | (name, Some a)::tl -> name ^ " = " ^ pretty_print_ast a ^ ",\n" ^ aux tl
  in let name, entries = s 
  in match entries with
  | [] -> name
  | l -> name ^ "{\n" ^ aux l ^ "}"

and pretty_print_decl_spec s = __print_list pretty_print_declaration_specifiers " " s

and pretty_print_decl decl name =  match decl with
  | DeBasic -> name
  | DeRecursive (a, b) ->  
    let decl_spec_print = pretty_print_decl_spec a
    in "(" ^
    (if String.length decl_spec_print != 0 then decl_spec_print ^ " " else "" ) ^
                            pretty_print_decl b name ^ ")"
  | DeArray (decl, spec, size) ->
    let decl_spec_print = pretty_print_decl_spec spec
    in pretty_print_decl decl name ^ "[" ^ 
    (if String.length decl_spec_print != 0 then decl_spec_print ^ " " else "" ) ^
    (match size with 
     | DeArraySize a -> pretty_print_ast a
     | DeArrayVla    -> "*"
     | DeArrayUndef  -> ""
    )
    ^ "]"
  | DeFunction (decl, params) ->
    pretty_print_decl decl name ^ "(" ^ 
    pretty_print_decl_params params 
    ^ ")"
and pretty_print_decl_type t = 
  let name, specs, decl = t 
  in pretty_print_decl_spec specs ^ " " ^ pretty_print_decl decl (fst name)

and pretty_print_decl_params params =
  let temp params = match params with
    | DeIdentifier s  -> s 
    | DeOthers  ->   "..." 
    | DeParam (spec, None) -> pretty_print_decl_spec spec 
    | DeParam (spec, Some t) -> pretty_print_decl_spec spec ^ " " ^ pretty_print_decl_type t 
  in __print_list temp ", " params




let pretty_print program =
  print_string (__print_list pretty_print_ast ";\n" program)
